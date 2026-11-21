package org.hswebframework.ezorm.rdb.supports.kingbaseES.oracle;

import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.supports.kingbaseES.AbstractKingbaseR2dbcIntegrationTests;
import org.hswebframework.ezorm.rdb.supports.kingbaseES.KingbaseTestContainer;
import org.junit.Assert;
import org.junit.ClassRule;
import org.junit.Test;

import java.time.Duration;
import java.time.LocalDate;
import java.sql.JDBCType;
import java.util.List;
import java.util.Map;
import java.util.Arrays;
import java.util.stream.Collectors;

public class KingbaseOracleR2dbcIntegrationTests extends AbstractKingbaseR2dbcIntegrationTests {

    @ClassRule
    public static final KingbaseTestContainer DATABASE = new KingbaseTestContainer("oracle");

    @Test
    public void shouldMapActualDatabaseTypesUsingOracleMappings() {
        assertDatabaseTypeMappings(false);
    }

    @Test
    public void shouldCreateAndReadTypesThroughOrmApi() {
        assertDatabaseTypeMappings(true);
    }

    private void assertDatabaseTypeMappings(boolean createThroughOrmApi) {
        Duration timeout = Duration.ofSeconds(30);
        // Supplemental type contracts; official assertions live in the Official* suites.
        // Expected JDBC/Java values are fixed and never computed by either dialect.
        Object[][] columns = {
            {"char_value", "char(32)", JDBCType.CHAR, String.class},
            {"nchar_value", "nchar(32)", JDBCType.CHAR, String.class},
            {"varchar_value", "varchar2(32 char)", JDBCType.VARCHAR, String.class},
            {"nvarchar_value", "nvarchar2(32)", JDBCType.VARCHAR, String.class},
            {"timestamp_value", "timestamp", JDBCType.TIMESTAMP, java.sql.Timestamp.class},
            {"date_value", "date", JDBCType.TIMESTAMP, java.sql.Date.class},
            {"tinyint_value", "number(1)", JDBCType.NUMERIC, java.math.BigDecimal.class},
            {"smallint_value", "number(5)", JDBCType.NUMERIC, java.math.BigDecimal.class},
            {"integer_value", "number(20)", JDBCType.NUMERIC, java.math.BigDecimal.class},
            {"bigint_value", "number(38,0)", JDBCType.NUMERIC, java.math.BigDecimal.class},
            {"decimal_value", "number(12,2)", JDBCType.NUMERIC, java.math.BigDecimal.class},
            {"double_value", "binary_double", JDBCType.DOUBLE, Double.class},
            {"float_value", "float", JDBCType.DOUBLE, Double.class},
            {"real_value", "real", JDBCType.REAL, String.class},
            {"clob_value", "clob", JDBCType.LONGVARCHAR, String.class},
            {"blob_value", "blob", JDBCType.LONGVARBINARY, byte[].class}
        };
        RDBTableMetadata probe = table.getSchema().newTable("types_" + table.getName());
        String definitions = Arrays.stream(columns)
            .map(column -> probe.getDialect().quote((String) column[0], false) + " " + column[1])
            .collect(Collectors.joining(", "));
        if (createThroughOrmApi) {
            DefaultDatabaseOperator.of(probe.getSchema().getDatabase()).ddl().createOrAlter(probe.getName())
                .addColumn("char_value").type(JDBCType.CHAR, String.class).length(32).commit()
                .addColumn("nchar_value").type(JDBCType.NCHAR, String.class).length(32).commit()
                .addColumn("varchar_value").varchar(32).commit()
                .addColumn("nvarchar_value").type(JDBCType.NVARCHAR, String.class).length(32).commit()
                .addColumn("timestamp_value").datetime().commit()
                .addColumn("date_value").type(JDBCType.DATE, LocalDate.class).commit()
                .addColumn("tinyint_value").tinyint().commit()
                .addColumn("smallint_value").type(JDBCType.SMALLINT, Short.class).commit()
                .addColumn("integer_value").integer().commit()
                .addColumn("bigint_value").bigint().commit()
                .addColumn("decimal_value").number(12, 2).commit()
                .addColumn("double_value").type(JDBCType.DOUBLE, Double.class).commit()
                .addColumn("float_value").type(JDBCType.FLOAT, Float.class).commit()
                .addColumn("real_value").type(JDBCType.REAL, Float.class).commit()
                .addColumn("clob_value").clob().commit()
                .addColumn("blob_value").type(JDBCType.BLOB, byte[].class).commit()
                .custom(metadata -> {
                    for (Object[] definition : columns) {
                        Assert.assertEquals(definition[0].toString(), definition[1], metadata.getDialect()
                            .buildColumnDataType(metadata.getColumnNow((String) definition[0])));
                    }
                })
                .commit().reactive().block(timeout);
        } else {
            executor.execute(SqlRequests.of("create table " + probe.getFullName() + " (" + definitions + ")"))
                .block(timeout);
        }
        try {
            List<Map<String, Object>> raw = executor.select(SqlRequests.of(
                "select column_name, data_type from information_schema.columns "
                    + "where table_schema = ? and table_name = ? order by ordinal_position",
                probe.getSchema().getName(), probe.getName()), ResultWrappers.lowerCase(ResultWrappers.map()))
                .collectList().block(timeout);
            Assert.assertNotNull(raw);
            Assert.assertEquals(columns.length, raw.size());
            // A fresh schema forces getTableReactive to query the database, avoiding the DDL metadata cache.
            RDBSchemaMetadata reloadedSchema = createSchema(probe.getSchema().getName());
            RDBDatabaseMetadata reloadedDatabase = new RDBDatabaseMetadata(reloadedSchema.getDialect());
            reloadedDatabase.addSchema(reloadedSchema);
            reloadedDatabase.setCurrentSchema(reloadedSchema);
            reloadedDatabase.addFeature(executor);
            RDBTableMetadata parsed = reloadedSchema.getTableReactive(probe.getName(), true).block(timeout);
            Assert.assertNotNull(parsed);
            Assert.assertEquals(columns.length, parsed.getColumns().size());
            for (int i = 0; i < columns.length; i++) {
                Object[] definition = columns[i];
                RDBColumnMetadata actual = parsed.getColumnNow((String) definition[0]);
                System.out.println("[实库类型映射] 字段=" + definition[0] + "，建列类型=" + definition[1]
                    + "，数据库返回=" + raw.get(i).get("data_type") + "，复用 Oracle=" + definition[2]
                    + "，JDBC=" + actual.getSqlType() + "，Java=" + actual.getJavaType().getName());
                Assert.assertEquals(definition[0].toString(), definition[2], actual.getSqlType());
                Assert.assertEquals(definition[0].toString(), definition[3], actual.getJavaType());
                if (i < 4) {
                    Assert.assertEquals(definition[0].toString(), 32, actual.getLength());
                }
            }
            Assert.assertEquals(12, parsed.getColumnNow("decimal_value").getPrecision());
            Assert.assertEquals(2, parsed.getColumnNow("decimal_value").getScale());
            System.out.println("[验证通过] 建表方式=" + (createThroughOrmApi ? "ORM API" : "原生 SQL")
                + "，16 种建列定义已通过 ORM API 实际回查，类型、长度、精度及 DATE/TIMESTAMP 区分正确");
        } finally {
            executor.execute(SqlRequests.of("drop table " + probe.getFullName())).block(timeout);
        }
    }

    @Test
    public void shouldExecuteStoredProcedureWithInputParameters() {
        Duration timeout = Duration.ofSeconds(30);
        String procedureName = table.getDialect().quote(table.getSchema().getName(), false)
            + "." + table.getDialect().quote("proc_" + table.getName());
        String createSql = String.join(" ",
            "create procedure " + procedureName,
            "(p_id in varchar2, p_name in varchar2, p_increment in number)",
            "as begin",
            "update " + table.getFullName(),
            "set \"NAME\" = p_name, \"POSITION_NO\" = \"POSITION_NO\" + p_increment",
            "where \"ID\" = p_id;",
            "end;");

        String insertSql = "insert into " + table.getFullName()
            + " (\"ID\", \"NAME\", \"POSITION_NO\") values (?, ?, ?)";
        Assert.assertEquals(Integer.valueOf(1),
            executor.update(insertSql, "target", "before", 10).block(timeout));
        Assert.assertEquals(Integer.valueOf(1),
            executor.update(insertSql, "control", "unchanged", 20).block(timeout));

        System.out.println("[存储过程创建] 模式=oracle，SQL=" + createSql);
        executor.execute(SqlRequests.of(createSql)).block(timeout);
        try {
            String newName = "已更新 'quoted'";
            String callSql = "call " + procedureName + "(?, ?, ?)";
            System.out.println("[存储过程调用] 模式=oracle，SQL=" + callSql
                + "，IN 参数：id=target，name=" + newName + "，increment=5");
            executor.execute(SqlRequests.of(callSql, "target", newName, 5)).block(timeout);

            List<Map<String, Object>> rows = executor.select(
                SqlRequests.of("select \"ID\", \"NAME\", \"POSITION_NO\" from "
                    + table.getFullName() + " order by \"ID\""),
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
            System.out.println("[存储过程验证通过] 模式=oracle，IN 参数已绑定并执行，目标记录=" + updated
                + "，未匹配记录=" + control);
        } finally {
            executor.execute(SqlRequests.of("drop procedure " + procedureName)).block(timeout);
            System.out.println("[测试清理] 已删除存储过程=" + procedureName);
        }
    }

    @Override
    protected String mode() {
        return "oracle";
    }

    @Override
    protected String defaultUrl() {
        return "r2dbc:kingbase://127.0.0.1:54322/test?mode=oracle";
    }

    @Override
    protected RDBSchemaMetadata createSchema(String schemaName) {
        return new KingbaseOracleSchemaMetadata(schemaName);
    }
}
