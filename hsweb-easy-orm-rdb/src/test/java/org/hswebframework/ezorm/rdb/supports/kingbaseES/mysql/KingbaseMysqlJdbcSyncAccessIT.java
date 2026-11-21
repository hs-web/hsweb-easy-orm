package org.hswebframework.ezorm.rdb.supports.kingbaseES.mysql;

import com.kingbase8.ds.KBSimpleDataSource;
import lombok.SneakyThrows;
import org.hswebframework.ezorm.rdb.ConnectionProvider;
import org.hswebframework.ezorm.rdb.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.mapping.SyncRepository;
import org.hswebframework.ezorm.rdb.mapping.defaults.record.Record;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.junit.Before;
import org.junit.Test;

import java.sql.Connection;
import java.util.List;
import java.util.UUID;

import static org.junit.Assert.assertEquals;

/** JDBC 同步接口示例：在 JUnit 中直接运行，保留演示表和数据。 */
public class KingbaseMysqlJdbcSyncAccessIT {

    private static final String TABLE = "jdbc_sync_demo_user";
    private SyncRepository<Record, String> repository;
    private String jdbcUrl;

    @Before
    public void setUp() {
        jdbcUrl = System.getProperty("kingbase.mysql.jdbc.url", "jdbc:kingbase8://127.0.0.1:54321/test");
        KBSimpleDataSource source = new KBSimpleDataSource();
        source.setUrl(jdbcUrl);
        source.setUser(System.getProperty("kingbase.jdbc.username", "system"));
        source.setPassword(System.getProperty("kingbase.jdbc.password", "1234"));

        // 仅注册 JDBC 同步执行器，连接在每次同步调用结束时关闭。
        TestSyncSqlExecutor executor = new TestSyncSqlExecutor(new ConnectionProvider() {
            @Override
            @SneakyThrows
            public Connection getConnection() {
                return source.getConnection();
            }

            @Override
            @SneakyThrows
            public void releaseConnect(Connection connection) {
                connection.close();
            }
        });
        KingbaseMysqlSchemaMetadata schema = new KingbaseMysqlSchemaMetadata("public");
        RDBDatabaseMetadata metadata = new RDBDatabaseMetadata(schema.getDialect());
        metadata.setCurrentSchema(schema);
        metadata.addSchema(schema);
        metadata.addFeature(executor);
        DefaultDatabaseOperator operator = DefaultDatabaseOperator.of(metadata);

        operator.ddl().createOrAlter(TABLE)
            .addColumn("id").primaryKey().varchar(64).commit()
            .addColumn("username").varchar(64).commit()
            .commit().sync();
        repository = operator.dml().createRepository(TABLE);
    }

    @Test
    public void insertsQueriesAndUpdatesThroughJdbcSyncRepository() {
        String id = "jdbc-sync-" + UUID.randomUUID();

        // 同步新增：方法返回时，插入操作已完成。
        repository.insert(repository.newInstance().putValue("id", id).putValue("username", "sync_user"));

        // 同步查询直接返回 Optional<Record>，无需订阅。
        Record inserted = repository.findById(id).orElseThrow(() -> new AssertionError("新增记录不存在"));
        assertEquals("sync_user", inserted.getString("username").orElseThrow());

        // 同步更新直接返回影响行数。
        int updated = repository.updateById(id, repository.newInstance().putValue("username", "sync_user_updated"));
        assertEquals(1, updated);

        // 同步条件查询直接返回 List<Record>。
        List<Record> records = repository.createQuery().where("id", id).fetch();
        assertEquals(1, records.size());
        assertEquals(id, records.get(0).getString("id").orElseThrow());
        assertEquals("sync_user_updated", records.get(0).getString("username").orElseThrow());

        System.out.println("JDBC 同步新增、查询及更新验证通过，记录已保留。数据库：" + jdbcUrl);
        System.out.println("SELECT * FROM public.\"" + TABLE + "\" WHERE id = '" + id + "';");
    }
}
