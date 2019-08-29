package org.hswebframework.ezorm.rdb.supports.mssql;

import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;

import java.util.HashMap;
import java.util.Map;

/**
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


    public SqlRequest buildDropTableCommentSql(String tableName, String comment) {
        Map<String, Object> param = new HashMap<>();
        param.put("table", tableName);
        param.put("comment", comment);
        return SqlRequests.template(dropTableCommentSql, param);
    }

    public SqlRequest buildTableCommentSql(String tableName, String comment) {
        Map<String, Object> param = new HashMap<>();
        param.put("table", tableName);
        param.put("comment", comment);
        return SqlRequests.template(isDropCommentBefore() ? dropAndCreateTableComment : tableCommentSql, param);
    }

    public SqlRequest buildColumnCommentSql(String tableName, String column, String comment) {
        Map<String, Object> param = new HashMap<>();
        param.put("table", tableName);
        param.put("comment", comment);
        param.put("column", column);
        return SqlRequests.template(isDropCommentBefore() ? dropAndCreateColumnComment : columnCommentSql, param);
    }

    public SqlRequest buildDropColumnCommentSql(String tableName, String column, String comment) {
        Map<String, Object> param = new HashMap<>();
        param.put("table", tableName);
        param.put("comment", comment);
        param.put("column", column);
        return SqlRequests.template(dropColumnCommentSql, param);
    }
}
