package org.hswebframework.ezorm.rdb.supports.kingbaseES.mysql;

import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBIndexMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.CreateTableSqlBuilder;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlCreateTableSqlBuilder;
import org.hswebframework.ezorm.rdb.supports.kingbaseES.AbstractKingbaseR2dbcIntegrationTests;
import org.hswebframework.ezorm.rdb.supports.kingbaseES.KingbaseTestContainer;
import org.junit.Assert;
import org.junit.ClassRule;
import org.junit.Test;

import java.time.Duration;
import java.sql.JDBCType;
import java.util.List;
import java.util.Map;

public class KingbaseMysqlR2dbcIntegrationTests extends AbstractKingbaseR2dbcIntegrationTests {

    @ClassRule
    public static final KingbaseTestContainer DATABASE = new KingbaseTestContainer("mysql");

    @Test
    public void shouldExecuteNativeMysqlCreateTableWithOptionsCommentsAndIndex() {
        Duration timeout = Duration.ofSeconds(30);
        RDBSchemaMetadata schema = table.getSchema();
        RDBTableMetadata nativeTable = schema.newTable(table.getName() + "_native_ddl");
        nativeTable.setComment("Native MySQL DDL test");

        RDBColumnMetadata id = nativeTable.newColumn();
        id.setName("id");
        id.setJdbcType(JDBCType.INTEGER, Integer.class);
        id.setPrimaryKey(true);
        id.setAutoIncrement(true);
        nativeTable.addColumn(id);

        RDBColumnMetadata name = nativeTable.newColumn();
        name.setName("name");
        name.setJdbcType(JDBCType.VARCHAR, String.class);
        name.setLength(50);
        name.setNotNull(true);
        name.setComment("Display name");
        nativeTable.addColumn(name);

        RDBIndexMetadata index = new RDBIndexMetadata("idx_" + nativeTable.getName());
        index.setUnique(true);
        index.getColumns().add(RDBIndexMetadata.IndexColumn.of("name", RDBIndexMetadata.IndexSort.asc));
        nativeTable.addIndex(index);

        CreateTableSqlBuilder builder = schema.findFeatureNow(CreateTableSqlBuilder.ID);
        Assert.assertTrue(builder instanceof MysqlCreateTableSqlBuilder);
        SqlRequest request = builder.build(nativeTable);
        String sql = request.getSql().replaceAll("\\s+", " ").trim();
        Assert.assertTrue(sql, sql.contains("ENGINE= InnoDB DEFAULT CHARSET= utf8mb4"));
        Assert.assertTrue(sql, sql.contains("comment 'Display name'"));
        Assert.assertTrue(sql, sql.contains("COMMENT= 'Native MySQL DDL test'"));
        System.out.println("[原生 MySQL 建表复用] SQL=" + request);
        try {
            executor.execute(request).block(timeout);
            Assert.assertEquals(Integer.valueOf(1), executor.update(
                "insert into " + nativeTable.getFullName() + " (`name`) values (?)", "created"
            ).block(timeout));
            List<Map<String, Object>> rows = executor.select(
                SqlRequests.of("select id, name from " + nativeTable.getFullName()),
                ResultWrappers.lowerCase(ResultWrappers.map())
            ).collectList().block(timeout);
            Assert.assertNotNull(rows);
            Assert.assertEquals(1, rows.size());
            Assert.assertEquals(1, ((Number) rows.get(0).get("id")).intValue());
            Assert.assertEquals("created", rows.get(0).get("name"));

            List<Map<String, Object>> comments = executor.select(SqlRequests.of(
                "select obj_description(c.oid, 'pg_class') as table_comment, "
                    + "col_description(c.oid, a.attnum) as column_comment "
                    + "from pg_class c join pg_namespace n on n.oid = c.relnamespace "
                    + "join pg_attribute a on a.attrelid = c.oid "
                    + "where n.nspname = ? and c.relname = ? and a.attname = ?",
                schema.getName(), nativeTable.getName(), "name"),
                ResultWrappers.lowerCase(ResultWrappers.map())
            ).collectList().block(timeout);
            Assert.assertNotNull(comments);
            Assert.assertEquals(1, comments.size());
            Assert.assertEquals("Native MySQL DDL test", comments.get(0).get("table_comment"));
            Assert.assertEquals("Display name", comments.get(0).get("column_comment"));

            RDBTableMetadata reloaded = schema.getTableReactive(nativeTable.getName(), true).block(timeout);
            Assert.assertNotNull(reloaded);
            Assert.assertEquals("Display name", reloaded.getColumnNow("name").getComment());
            Assert.assertEquals(50, reloaded.getColumnNow("name").getLength());
            Assert.assertTrue(reloaded.getIndexes().stream().anyMatch(
                actual -> index.getName().equals(actual.getName()) && actual.isUnique()));
            System.out.println("[验证通过] 引擎及字符集选项、内联注释、自增主键、索引批次均可执行");
        } finally {
            executor.execute(SqlRequests.of("drop table if exists " + nativeTable.getFullName())).block(timeout);
        }
    }

    @Test
    public void shouldReadColumnTypesUsingNativeMysqlMappings() {
        Duration timeout = Duration.ofSeconds(30);
        // Supplemental native-MySQL comparison, distinct from the inherited upstream suites.
        // Fixed expectations captured from MySQL 8.4.11 with upstream mapping at 96b61b0b.
        Object[][] definitions = {
            {"char(32)", JDBCType.OTHER, String.class},
            {"varchar(32)", JDBCType.VARCHAR, String.class},
            {"nchar(32)", JDBCType.OTHER, String.class},
            {"nvarchar(32)", JDBCType.VARCHAR, String.class},
            {"tinytext", JDBCType.OTHER, String.class},
            {"text", JDBCType.CLOB, String.class},
            {"mediumtext", JDBCType.OTHER, String.class},
            {"longtext", JDBCType.LONGVARCHAR, String.class},
            {"binary(16)", JDBCType.BINARY, byte[].class},
            {"varbinary(16)", JDBCType.OTHER, String.class},
            {"tinyblob", JDBCType.OTHER, String.class},
            {"blob", JDBCType.BLOB, String.class},
            {"mediumblob", JDBCType.OTHER, String.class},
            {"longblob", JDBCType.OTHER, String.class},
            {"enum('a','b')", JDBCType.VARCHAR, String.class},
            {"set('a','b')", JDBCType.VARCHAR, String.class},
            {"json", JDBCType.OTHER, String.class},
            {"tinyint", JDBCType.TINYINT, Byte.class},
            {"smallint", JDBCType.SMALLINT, String.class},
            {"mediumint", JDBCType.OTHER, String.class},
            {"int", JDBCType.INTEGER, Integer.class},
            {"integer", JDBCType.INTEGER, Integer.class},
            {"bigint", JDBCType.BIGINT, Long.class},
            {"decimal(12,2)", JDBCType.DECIMAL, java.math.BigDecimal.class},
            {"numeric(12,2)", JDBCType.DECIMAL, java.math.BigDecimal.class},
            {"float", JDBCType.FLOAT, String.class},
            {"float(12,2)", JDBCType.OTHER, String.class},
            {"float(10)", JDBCType.FLOAT, String.class},
            {"float(30)", JDBCType.DOUBLE, Double.class},
            {"double", JDBCType.DOUBLE, Double.class},
            {"double(12,2)", JDBCType.DOUBLE, Double.class},
            {"double precision", JDBCType.DOUBLE, Double.class},
            {"real", JDBCType.DOUBLE, Double.class},
            {"bit(1)", JDBCType.OTHER, String.class},
            {"bit(8)", JDBCType.OTHER, String.class},
            {"boolean", JDBCType.TINYINT, Byte.class},
            {"bool", JDBCType.TINYINT, Byte.class},
            {"date", JDBCType.DATE, java.time.LocalDate.class},
            {"time(6)", JDBCType.TIME, java.time.LocalTime.class},
            {"datetime(6)", JDBCType.TIMESTAMP, java.sql.Date.class},
            {"timestamp(6)", JDBCType.TIMESTAMP, java.sql.Timestamp.class},
            {"year", JDBCType.DATE, java.sql.Date.class},
            {"tinyint unsigned", JDBCType.OTHER, String.class},
            {"smallint unsigned", JDBCType.OTHER, String.class},
            {"mediumint unsigned", JDBCType.OTHER, String.class},
            {"int unsigned", JDBCType.OTHER, String.class},
            {"bigint unsigned", JDBCType.OTHER, String.class},
            {"int(11)", JDBCType.INTEGER, Integer.class},
            {"tinyint(1)", JDBCType.TINYINT, Byte.class},
            {"int(11) unsigned", JDBCType.OTHER, String.class}
        };
        RDBTableMetadata types = table.getSchema().newTable(table.getName() + "_types");
        StringBuilder sql = new StringBuilder("create table ").append(types.getFullName()).append(" (");
        for (int i = 0; i < definitions.length; i++) {
            if (i > 0) {
                sql.append(", ");
            }
            sql.append("c").append(i).append(" ").append(definitions[i][0]);
        }
        sql.append(")");
        executor.execute(SqlRequests.of(sql.toString())).block(timeout);
        try {
            RDBTableMetadata reloaded = table.getSchema().getTableReactive(types.getName(), true).block(timeout);
            Assert.assertNotNull(reloaded);
            Assert.assertEquals(definitions.length, reloaded.getColumns().size());
            for (int i = 0; i < definitions.length; i++) {
                RDBColumnMetadata column = reloaded.getColumnNow("c" + i);
                Assert.assertEquals(String.valueOf(definitions[i][0]), definitions[i][1], column.getSqlType());
                Assert.assertEquals(String.valueOf(definitions[i][0]), definitions[i][2], column.getJavaType());
                System.out.println("[MySQL 类型映射一致] 定义=" + definitions[i][0]
                    + "，JDBC=" + column.getSqlType() + "，Java=" + column.getJavaType().getName());
            }
        } finally {
            executor.execute(SqlRequests.of("drop table " + types.getFullName())).block(timeout);
        }
    }

    @Test
    public void shouldExecuteStoredProcedureWithInputParameters() {
        Duration timeout = Duration.ofSeconds(30);
        String procedureName = table.getDialect().quote(table.getSchema().getName(), false)
            + "." + table.getDialect().quote("proc_" + table.getName());
        String createSql = String.join(" ",
            "create procedure " + procedureName,
            "(in p_id varchar(32), in p_name varchar(64), in p_increment int)",
            "begin",
            "update " + table.getFullName(),
            "set `name` = p_name, `position_no` = `position_no` + p_increment",
            "where `id` = p_id;",
            "end");

        String insertSql = "insert into " + table.getFullName()
            + " (`id`, `name`, `position_no`) values (?, ?, ?)";
        Assert.assertEquals(Integer.valueOf(1),
            executor.update(insertSql, "target", "before", 10).block(timeout));
        Assert.assertEquals(Integer.valueOf(1),
            executor.update(insertSql, "control", "unchanged", 20).block(timeout));

        try {
            System.out.println("[存储过程创建] 模式=mysql，SQL=" + createSql);
            executor.execute(SqlRequests.of(createSql)).block(timeout);

            String newName = "已更新 'quoted'";
            String callSql = "call " + procedureName + "(?, ?, ?)";
            System.out.println("[存储过程调用] SQL=" + callSql
                + "，IN 参数：id=target，name=" + newName + "，increment=5");
            executor.execute(SqlRequests.of(callSql, "target", newName, 5)).block(timeout);

            List<Map<String, Object>> rows = executor.select(
                SqlRequests.of("select id, name, position_no from " + table.getFullName() + " order by id"),
                ResultWrappers.lowerCase(ResultWrappers.map())
            ).collectList().block(timeout);
            Assert.assertNotNull(rows);
            Assert.assertEquals(2, rows.size());
            Map<String, Object> control = rows.get(0);
            Map<String, Object> updated = rows.get(1);
            Assert.assertEquals("control", control.get("id"));
            Assert.assertEquals("unchanged", control.get("name"));
            Assert.assertEquals(20, ((Number) control.get("position_no")).intValue());
            Assert.assertEquals("target", updated.get("id"));
            Assert.assertEquals(newName, updated.get("name"));
            Assert.assertEquals(15, ((Number) updated.get("position_no")).intValue());
            System.out.println("[存储过程验证通过] IN 参数已绑定并执行，目标记录=" + updated
                + "，未匹配记录=" + control);
        } finally {
            executor.execute(SqlRequests.of("drop procedure if exists " + procedureName)).block(timeout);
            System.out.println("[测试清理] 已删除存储过程=" + procedureName);
        }
    }

    @Override
    protected String mode() {
        return "mysql";
    }

    @Override
    protected String defaultUrl() {
        return "r2dbc:kingbase://127.0.0.1:54321/test?mode=mysql";
    }

    @Override
    protected RDBSchemaMetadata createSchema(String schemaName) {
        return new KingbaseMysqlSchemaMetadata(schemaName);
    }
}
