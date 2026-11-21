package org.hswebframework.ezorm.rdb.supports.kingbaseES.oracle;

import org.hswebframework.ezorm.core.OriginalValueCodec;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.wrapper.MapResultWrapper;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers;
import org.hswebframework.ezorm.rdb.mapping.ReactiveRepository;
import org.hswebframework.ezorm.rdb.mapping.defaults.record.Record;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.supports.kingbaseES.KingbaseOfficialTestDatabase;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;
import reactor.core.publisher.Flux;

import java.io.ByteArrayOutputStream;
import java.nio.ByteBuffer;
import java.sql.JDBCType;
import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.Map;

/** Regression tests for Oracle-compatible LocalTime binding and BLOB decoding. */
public class KingbaseOracleDriverRegressionTests {

    @ClassRule
    public static final KingbaseOfficialTestDatabase DATABASE = new KingbaseOfficialTestDatabase("oracle");

    private static final Duration TIMEOUT = Duration.ofSeconds(30);
    private static DatabaseOperator operator;
    private static RDBSchemaMetadata schema;

    @BeforeClass
    public static void createTables() throws Exception {
        System.out.println("[驱动构件] " + Class.forName("io.r2dbc.postgresql.PostgresqlConnectionFactory")
            .getProtectionDomain().getCodeSource().getLocation());
        schema = DATABASE.newSchema();
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(schema.getDialect());
        database.addSchema(schema);
        database.setCurrentSchema(schema);
        database.addFeature(DATABASE.reactive());
        operator = DefaultDatabaseOperator.of(database);
        operator.ddl().createOrAlter("driver_time_probe")
            .addColumn("id").varchar(32).primaryKey().commit()
            .addColumn("time_value").type(JDBCType.TIME, LocalTime.class).commit()
            .commit().reactive().block(TIMEOUT);
        operator.ddl().createOrAlter("driver_blob_probe")
            .addColumn("id").varchar(32).primaryKey().commit()
            .addColumn("payload").type(JDBCType.BLOB, byte[].class).commit()
            .commit().reactive().block(TIMEOUT);
    }

    @Test
    public void shouldBindLocalTimeToTimestamp() {
        LocalTime input = LocalTime.of(10, 30, 15, 123456000);
        ReactiveRepository<Record, String> repository = operator.dml().createReactiveRepository("driver_time_probe");
        Assert.assertEquals(Integer.valueOf(1), repository.insert(Record.newRecord()
            .putValue("id", "local-time").putValue("time_value", input)).block(TIMEOUT));
        // Read the driver's value directly, without the ORM's millisecond-based DateTimeCodec.
        Object actual = selectValue("driver_time_probe", "TIME_VALUE", "local-time");
        Assert.assertEquals(LocalDateTime.of(LocalDate.EPOCH, input), actual);
        System.out.println("[LocalTime 通过] " + input + " -> " + actual);
    }

    @Test
    public void shouldBindNullLocalTimeToTimestamp() {
        ReactiveRepository<Record, String> repository = operator.dml().createReactiveRepository("driver_time_probe");
        // The ORM binds an omitted TIME column with LocalTime.class, reproducing the typed NULL failure.
        Assert.assertEquals(Integer.valueOf(1), repository.insert(Record.newRecord()
            .putValue("id", "null-time")).block(TIMEOUT));
        Assert.assertNull(selectValue("driver_time_probe", "TIME_VALUE", "null-time"));
        System.out.println("[LocalTime NULL 通过] 已写入并读取 NULL");
    }

    @Test
    public void shouldReadBlobContents() {
        byte[] large = new byte[65537];
        for (int i = 0; i < large.length; i++) {
            large[i] = (byte) (i * 31);
        }
        byte[][] payloads = {new byte[]{0, 1, (byte) 0xff, 42}, large};
        ReactiveRepository<Record, String> repository = operator.dml().createReactiveRepository("driver_blob_probe");
        for (int i = 0; i < payloads.length; i++) {
            String id = "blob-" + i;
            byte[] expected = payloads[i];
            Assert.assertEquals(Integer.valueOf(1), repository.insert(Record.newRecord()
                .putValue("id", id).putValue("payload", expected)).block(TIMEOUT));
            Object driverValue = selectValue("driver_blob_probe", "PAYLOAD", id);
            if (i == 0) {
                RDBTableMetadata table = schema.getTable("driver_blob_probe", false).orElseThrow(AssertionError::new);
                Map<String, Object> storage = DATABASE.reactive().select(SqlRequests.of(
                    "select cast(pg_typeof(\"PAYLOAD\") as text) as \"storage_type\" from "
                        + table.getFullName() + " where \"ID\" = ?", id),
                    ResultWrappers.lowerCase(ResultWrappers.map())).single().block(TIMEOUT);
                System.out.println("[BLOB 列类型] " + storage + "，返回 Java 类型="
                    + (driverValue == null ? "null" : driverValue.getClass().getName()));
            }
            Assert.assertArrayEquals(id + ": driver read", expected, bytes(driverValue));
            Record loaded = repository.findById(id).block(TIMEOUT);
            Assert.assertNotNull(loaded);
            Assert.assertArrayEquals(id + ": ORM read", expected, bytes(loaded.getObject("payload")));
            System.out.println("[BLOB 通过] " + id + "，字节数=" + expected.length + "，驱动及 ORM 回读内容一致");
        }
    }

    @Test
    public void shouldReadNullBlob() {
        ReactiveRepository<Record, String> repository = operator.dml().createReactiveRepository("driver_blob_probe");
        Assert.assertEquals(Integer.valueOf(1), repository.insert(Record.newRecord()
            .putValue("id", "null-blob")).block(TIMEOUT));
        Assert.assertNull(selectValue("driver_blob_probe", "PAYLOAD", "null-blob"));
        Record loaded = repository.findById("null-blob").block(TIMEOUT);
        Assert.assertNotNull(loaded);
        Assert.assertNull(loaded.getObject("payload"));
        System.out.println("[BLOB NULL 通过] 驱动及 ORM 均读取到 NULL");
    }

    private static Object selectValue(String tableName, String columnName, String id) {
        RDBTableMetadata table = schema.getTable(tableName, false).orElseThrow(AssertionError::new);
        // Keep the driver's binary value: the default map decoder interprets ByteBuffer as UTF-8 text.
        MapResultWrapper wrapper = new MapResultWrapper();
        wrapper.setCodec(OriginalValueCodec.INSTANCE);
        Map<String, Object> row = DATABASE.reactive().select(SqlRequests.of(
            "select \"" + columnName + "\" as \"value\" from " + table.getFullName() + " where \"ID\" = ?", id),
            ResultWrappers.lowerCase(wrapper)).single().block(TIMEOUT);
        Assert.assertNotNull(row);
        Assert.assertTrue(row.containsKey("value"));
        return row.get("value");
    }

    private static byte[] bytes(Object value) {
        Assert.assertNotNull("Expected a non-null BLOB", value);
        if (value instanceof byte[]) {
            return (byte[]) value;
        }
        if (value instanceof ByteBuffer) {
            ByteBuffer buffer = ((ByteBuffer) value).duplicate();
            byte[] bytes = new byte[buffer.remaining()];
            buffer.get(bytes);
            return bytes;
        }
        if (value instanceof io.r2dbc.spi.Blob) {
            ByteArrayOutputStream output = new ByteArrayOutputStream();
            Flux.from(((io.r2dbc.spi.Blob) value).stream())
                .doOnNext(buffer -> output.writeBytes(bytes(buffer))).then().block(TIMEOUT);
            return output.toByteArray();
        }
        String description = value instanceof String
            ? "codePoints=" + java.util.Arrays.toString(((String) value).codePoints().limit(16).toArray())
            : String.valueOf(value);
        throw new AssertionError("Unsupported BLOB result: " + value.getClass().getName()
            + ", value=" + description.substring(0, Math.min(80, description.length())));
    }
}
