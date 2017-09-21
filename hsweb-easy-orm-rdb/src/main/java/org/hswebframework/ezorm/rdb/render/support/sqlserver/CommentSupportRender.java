package org.hswebframework.ezorm.rdb.render.support.sqlserver;

import org.hswebframework.ezorm.rdb.executor.SQL;
import org.hswebframework.ezorm.rdb.render.support.simple.SimpleSQL;

import java.util.HashMap;
import java.util.Map;

/**
 * TODO 完成注释
 *
 * @author zhouhao
 */
public abstract class CommentSupportRender {

    protected abstract boolean isDropCommentBefore();

    private static final String dropTableCommentSql       = "IF exists(SELECT 1 FROM fn_listextendedproperty('MS_Description', 'SCHEMA', 'dbo', 'TABLE', #{table}, NULL, NULL) WHERE name = 'MS_Description') EXEC sp_dropextendedproperty 'MS_Description', 'SCHEMA', 'dbo', 'TABLE', #{table}";
    private static final String tableCommentSql           = "EXEC sp_addextendedproperty 'MS_Description', #{comment}, 'SCHEMA', 'dbo', 'TABLE', #{table}";
    private static final String dropAndCreateTableComment = dropTableCommentSql.concat(";").concat(tableCommentSql);

    private static final String dropColumnCommentSql       = "IF exists(SELECT 1 FROM fn_listextendedproperty('MS_Description', 'SCHEMA', 'dbo', 'TABLE', #{table}, 'column', #{column}) WHERE name = 'MS_Description') EXEC sp_dropextendedproperty 'MS_Description', 'SCHEMA', 'dbo', 'TABLE', #{table}, 'column', #{column}";
    private static final String columnCommentSql           = "EXEC sp_addextendedproperty 'MS_Description', #{comment}, 'SCHEMA', 'dbo', 'TABLE', #{table}, 'column', #{column}";
    private static final String dropAndCreateColumnComment = dropTableCommentSql.concat(";").concat(tableCommentSql);


    public SQL buildDropTableCommentSql(String tableName, String comment) {
        Map<String, Object> param = new HashMap<>();
        param.put("table", tableName);
        param.put("comment", comment);
        return new SimpleSQL(dropTableCommentSql, param);
    }

    public SQL buildTableCommentSql(String tableName, String comment) {
        Map<String, Object> param = new HashMap<>();
        param.put("table", tableName);
        param.put("comment", comment);
        return new SimpleSQL(isDropCommentBefore() ? dropAndCreateTableComment : tableCommentSql, param);
    }

    public SQL buildColumnCommentSql(String tableName, String column, String comment) {
        Map<String, Object> param = new HashMap<>();
        param.put("table", tableName);
        param.put("comment", comment);
        param.put("column", column);
        return new SimpleSQL(isDropCommentBefore() ? dropAndCreateColumnComment : columnCommentSql, param);
    }

    public SQL buildDropColumnCommentSql(String tableName, String column, String comment) {
        Map<String, Object> param = new HashMap<>();
        param.put("table", tableName);
        param.put("comment", comment);
        param.put("column", column);
        return new SimpleSQL(dropColumnCommentSql, param);
    }
}
