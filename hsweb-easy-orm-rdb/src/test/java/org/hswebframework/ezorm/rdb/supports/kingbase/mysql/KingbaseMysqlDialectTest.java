package org.hswebframework.ezorm.rdb.supports.kingbase.mysql;

import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.junit.Assert;
import org.junit.Test;

/**
 * Verifies the public SQL type mapping exposed for KingbaseES MySQL mode.
 */
public class KingbaseMysqlDialectTest {

    @Test
    public void shouldExposeKingbaseMysqlDialectAndVarcharMapping() {
        KingbaseMysqlDialect dialect = new KingbaseMysqlDialect();

        System.out.println("[方言信息] 标识=" + dialect.getId() + "，名称=" + dialect.getName());
        Assert.assertEquals("kingbase-mysql", dialect.getId());
        Assert.assertEquals("KingbaseMysql", dialect.getName());
        String quotedName = dialect.quote("account");
        System.out.println("[标识符引用] 输入=account，实际=" + quotedName);
        Assert.assertEquals("`account`", quotedName);

        RDBColumnMetadata column = new RDBColumnMetadata();
        column.setLength(64);
        DataType dataType = dialect.convertDataType("character varying");
        column.setType(dataType);
        System.out.println("[数据库类型识别] 输入=character varying，SQL类型="
            + dataType.getSqlType() + "，Java类型=" + dataType.getJavaType().getName());

        String columnType = dialect.buildColumnDataType(column);
        System.out.println("[字段类型生成] 输入长度=64，实际=" + columnType);
        Assert.assertEquals("varchar(64)", columnType);
    }
}
