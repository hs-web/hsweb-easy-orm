package org.hswebframework.ezorm.rdb.supports.mssql;

import org.hswebframework.ezorm.rdb.meta.RDBColumnMetaData;
import org.hswebframework.ezorm.rdb.supports.commons.CommonCreateIndexRender;
import org.hswebframework.utils.StringUtils;
import org.hswebframework.ezorm.rdb.executor.BindSQL;
import org.hswebframework.ezorm.rdb.meta.RDBTableMetaData;
import org.hswebframework.ezorm.rdb.executor.SQL;
import org.hswebframework.ezorm.rdb.render.SqlAppender;
import org.hswebframework.ezorm.rdb.render.SqlRender_;
import org.hswebframework.ezorm.rdb.supports.commons.SimpleSQL;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * sqlServer 表结构创建 sql渲染器,用于渲染sqlServer创建表的sql
 */
public class SqlServerMetaCreateRender extends CommentSupportRender implements SqlRender_ {
    @Override
    protected boolean isDropCommentBefore() {
        return false;
    }

    @Override
    public SQL render(RDBTableMetaData table, Object param) {
        SqlAppender createBody = new SqlAppender();
        List<SQL> comments = new ArrayList<>();
        Set<RDBColumnMetaData> RDBColumnMetaDatas = table.getColumns();
        if (RDBColumnMetaDatas.isEmpty()) throw new UnsupportedOperationException("未指定任何字段");
        createBody.add("\nCREATE TABLE ", table.getName(), "(");
        RDBColumnMetaDatas.forEach(column -> {
            createBody.add("\n\t[", column.getName(), "] ").add(column.getDataType());
            if (column.isNotNull()
                    || column.isPrimaryKey()) {
                createBody.add(" NOT NULL ");
            }
            if (column.isPrimaryKey())
                createBody.add("PRIMARY KEY ");
            //注释
            if (!StringUtils.isNullOrEmpty(column.getComment())) {
                comments.add(buildColumnCommentSql(table.getName(), (column.getName().toUpperCase()), column.getComment()));
            }
            createBody.add(",");
        });
        comments.add(buildTableCommentSql(table.getName(), table.getComment()));
        createBody.removeLast();
        createBody.add("\n)");
        SimpleSQL simpleSQL = new SimpleSQL(createBody.toString(), param);
        List<BindSQL> bindSQLs = comments.stream().map(s -> {
            BindSQL sql = new BindSQL();
            sql.setSql(s);
            return sql;
        }).collect(Collectors.toList());
        for (SQL sql : CommonCreateIndexRender.buildCreateIndexSql(table)) {
            bindSQLs.add(new BindSQL(sql));
        }
        simpleSQL.setBindSQLs(bindSQLs);
        return simpleSQL;
    }
}
