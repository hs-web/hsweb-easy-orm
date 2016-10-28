package org.hsweb.ezorm.rdb.render.support.mysql;

import org.hsweb.commons.StringUtils;
import org.hsweb.ezorm.rdb.executor.SQL;
import org.hsweb.ezorm.rdb.meta.RDBColumnMetaData;
import org.hsweb.ezorm.rdb.meta.RDBTableMetaData;
import org.hsweb.ezorm.rdb.render.SqlAppender;
import org.hsweb.ezorm.rdb.render.SqlRender;
import org.hsweb.ezorm.rdb.render.support.simple.SimpleSQL;

import java.util.Set;

/**
 * Created by zhouhao on 16-6-5.
 */
public class MysqlMetaCreateRender implements SqlRender {
    @Override
    public SQL render(RDBTableMetaData metaData, Object param) {
        SqlAppender appender = new SqlAppender();
        Set<RDBColumnMetaData> RDBColumnMetaDatas = metaData.getColumns();
        if (RDBColumnMetaDatas.isEmpty()) throw new UnsupportedOperationException("未指定任何字段");
        appender.add("\nCREATE TABLE ", metaData.getName(), "(");
        RDBColumnMetaDatas.forEach(fieldMetaData -> {
            appender.add("\n\t`", fieldMetaData.getName(), "` ").add(fieldMetaData.getDataType());
            if (fieldMetaData.getProperty("not-null", false).isTrue()) {
                appender.add(" not null ");
            }
            //注释
            if (!StringUtils.isNullOrEmpty(fieldMetaData.getComment())) {
                appender.add(String.format(" COMMENT '%s'", fieldMetaData.getComment()));
            } else {
                appender.add(String.format(" COMMENT '%s%s'", "列:", fieldMetaData.getAlias()));
            }
            appender.add(",");
        });
        appender.removeLast();
        if (!metaData.getPrimaryKeys().isEmpty()) {
            appender.add(",", "\n\tprimary key (");
            metaData.getPrimaryKeys().forEach(pk -> appender.add("`", pk, "`",","));
            appender.removeLast();
            appender.addEdSpc(")");
        }
        appender.add("\n)");
        if (metaData.getComment() != null) {
            appender.add("COMMENT=", "'", metaData.getComment(), "'");
        }
        return new SimpleSQL( appender.toString(), param);
    }
}
