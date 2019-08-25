package org.hswebframework.ezorm.rdb.supports.posgres;

import org.hswebframework.ezorm.rdb.executor.BindSQL;
import org.hswebframework.ezorm.rdb.executor.SQL;
import org.hswebframework.ezorm.rdb.meta.RDBColumnMetaData;
import org.hswebframework.ezorm.rdb.meta.RDBTableMetaData;
import org.hswebframework.ezorm.rdb.render.SqlAppender;
import org.hswebframework.ezorm.rdb.render.SqlRender;
import org.hswebframework.ezorm.rdb.supports.commons.CommonCreateIndexRender;
import org.hswebframework.ezorm.rdb.supports.commons.SimpleSQL;
import org.hswebframework.utils.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class PostgresMetaCreateRender implements SqlRender<Object> {
    @Override
    public SQL render(RDBTableMetaData metaData, Object param) {
        SqlAppender createBody = new SqlAppender();
        List<String> comments = new ArrayList<>();
        Set<RDBColumnMetaData> columns = metaData.getColumns();
        if (columns.isEmpty()) throw new UnsupportedOperationException("未指定任何字段");
        createBody.add("\nCREATE TABLE ", metaData.getFullName(), "(");
        columns.forEach(column -> {
            String columnFullName = metaData.getDatabaseMetaData().getDialect().buildColumnName(null, column.getName());

            createBody.add("\n\t", columnFullName, " ");

            if (column.getColumnDefinition() != null) {
                createBody.add(column.getColumnDefinition());

            } else {
                createBody.add(column.getDataType());
                if (column.isNotNull()
                        || column.isPrimaryKey()) {
                    createBody.add(" NOT NULL ");
                }
                if (column.isPrimaryKey())
                    createBody.add("PRIMARY KEY ");
                //注释
                if (!StringUtils.isNullOrEmpty(column.getComment())) {
                    comments.add(String.format("COMMENT ON COLUMN %s.%s IS '%s'", metaData.getFullName(), columnFullName, column.getComment()));
                }
            }
            createBody.add(",");
        });
        comments.add(String.format("COMMENT ON TABLE %s IS '%s'", metaData.getFullName(), metaData.getComment()));
        createBody.removeLast();
        createBody.add("\n)");
        SimpleSQL simpleSQL = new SimpleSQL(createBody.toString(), param);
        List<BindSQL> bindSQLs = comments
                .stream()
                .map(s -> {
                    BindSQL sql = new BindSQL();
                    sql.setSql(new SimpleSQL(s, param));
                    return sql;
                }).collect(Collectors.toList());

        for (SQL sql : CommonCreateIndexRender.buildCreateIndexSql(metaData)) {
            bindSQLs.add(new BindSQL(sql));
        }

        simpleSQL.setBindSQLs(bindSQLs);
        return simpleSQL;
    }
}