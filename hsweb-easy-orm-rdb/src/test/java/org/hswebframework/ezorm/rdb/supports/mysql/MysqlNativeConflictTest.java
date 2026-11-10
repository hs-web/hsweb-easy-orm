package org.hswebframework.ezorm.rdb.supports.mysql;

import org.hswebframework.ezorm.core.RuntimeDefaultValue;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.metadata.JdbcDataType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBIndexMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.dml.upsert.UpsertOperator;
import org.junit.Test;

import java.sql.JDBCType;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.Assert.*;

public class MysqlNativeConflictTest {

    @Test
    public void secondaryUniqueIndexRetainsConservativeFallbackByDefault() {
        Fixture fixture = new Fixture();
        fixture.upsert().columns("id", "code", "value")
               .values("1", "c1", "v1").values("2", "c2", "v2").execute().sync();
        assertEquals(2, fixture.executor.requests.size());
        assertTrue(fixture.executor.requests.stream().allMatch(sql -> sql.getSql().startsWith("update")));
    }

    @Test
    public void explicitNativeModeEmitsOneMultirowStatementWithUniqueIndex() {
        Fixture fixture = new Fixture();
        fixture.upsert().nativeConflict(true).columns("id", "code", "value")
               .values("1", "c1", "v1").values("2", "c2", "v2").execute().sync();
        assertEquals(1, fixture.executor.requests.size());
        SqlRequest request = fixture.executor.requests.get(0);
        assertTrue(request.getSql().contains("on duplicate key update"));
        assertEquals(6, request.getParameters().length);
    }

    @Test
    public void runtimeDefaultCopyRetainsNativeStrategy() {
        Fixture fixture = new Fixture();
        AtomicInteger sequence = new AtomicInteger();
        fixture.table.getColumnNow("id").setDefaultValue((RuntimeDefaultValue) () -> "g" + sequence.incrementAndGet());
        fixture.upsert().nativeConflict(true).columns("code", "value")
               .values("c1", "v1").values("c2", "v2").execute().sync();
        assertEquals(1, fixture.executor.requests.size());
        assertTrue(fixture.executor.requests.get(0).getSql().contains("on duplicate key update"));
        assertEquals(2, sequence.get());
    }

    @Test
    public void nativeModeRejectsUnsupportedUpdatePredicate() {
        Fixture fixture = new Fixture();
        UpsertOperator upsert = fixture.upsert().nativeConflict(true).columns("id", "code", "value")
                                          .values("1", "c1", "v1").where(where -> where.and("value", "eq", "old"));
        assertThrows(UnsupportedOperationException.class, upsert::execute);
        assertTrue(fixture.executor.requests.isEmpty());
    }

    private static final class Fixture {
        private final CapturingExecutor executor = new CapturingExecutor();
        private final DefaultDatabaseOperator operator;
        private final RDBTableMetadata table;

        private Fixture() {
            RDBDatabaseMetadata database = new RDBDatabaseMetadata(Dialect.MYSQL);
            MysqlSchemaMetadata schema = new MysqlSchemaMetadata("test");
            database.addFeature(executor);
            database.addSchema(schema);
            database.setCurrentSchema(schema);
            table = schema.newTable("native_conflict_test");
            table.addColumn(column("id", true));
            table.addColumn(column("code", false));
            table.addColumn(column("value", false));
            RDBIndexMetadata unique = new RDBIndexMetadata("uk_code");
            unique.setUnique(true);
            unique.setColumns(Collections.singletonList(RDBIndexMetadata.IndexColumn.of("code", RDBIndexMetadata.IndexSort.asc)));
            table.addIndex(unique);
            schema.addTable(table);
            operator = DefaultDatabaseOperator.of(database);
        }

        private UpsertOperator upsert() {
            return operator.dml().upsert(table);
        }
    }

    private static RDBColumnMetadata column(String name, boolean primary) {
        RDBColumnMetadata column = new RDBColumnMetadata();
        column.setName(name);
        column.setAlias(name);
        column.setPrimaryKey(primary);
        column.setLength(64);
        column.setType(JdbcDataType.of(JDBCType.VARCHAR, String.class));
        return column;
    }

    private static final class CapturingExecutor implements SyncSqlExecutor {
        private final List<SqlRequest> requests = new ArrayList<>();

        @Override
        public int update(SqlRequest request) {
            requests.add(request);
            return 1;
        }

        @Override
        public void execute(SqlRequest request) {
            requests.add(request);
        }

        @Override
        public <T, R> R select(SqlRequest request, ResultWrapper<T, R> wrapper) {
            throw new UnsupportedOperationException();
        }
    }
}
