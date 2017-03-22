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
            if (fieldMetaData.isNotNull()) {
                appender.add(" not null");
            }
            if (fieldMetaData.isPrimaryKey()) {
                appender.add(" primary key");
            }
            //注释
            if (!StringUtils.isNullOrEmpty(fieldMetaData.getComment())) {
                appender.add(String.format(" COMMENT '%s'", fieldMetaData.getComment()));
            }
            appender.add(",");
        });
        appender.removeLast();
        appender.add("\n)");
        if (metaData.getComment() != null) {
            appender.add("COMMENT=", "'", metaData.getComment(), "'");
        }
        return new SimpleSQL(appender.toString(), param);
    }
}
