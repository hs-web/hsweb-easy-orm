package org.hswebframework.ezorm.rdb.supports.kingbase.oracle;

import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.supports.kingbase.AbstractKingbaseR2dbcIntegrationTests;
import org.hswebframework.ezorm.rdb.supports.kingbase.KingbaseTestContainer;
import org.junit.Assert;
import org.junit.ClassRule;
import org.junit.Test;

import java.time.Duration;
import java.util.List;
import java.util.Map;

public class KingbaseOracleR2dbcIntegrationTests extends AbstractKingbaseR2dbcIntegrationTests {

    @ClassRule
    public static final KingbaseTestContainer DATABASE = new KingbaseTestContainer("oracle");

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
