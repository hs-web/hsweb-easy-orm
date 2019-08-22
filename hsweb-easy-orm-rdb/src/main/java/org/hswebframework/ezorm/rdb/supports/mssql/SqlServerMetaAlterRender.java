package org.hswebframework.ezorm.rdb.supports.mssql;

import org.hswebframework.ezorm.rdb.executor.EmptySQL;
import org.hswebframework.ezorm.rdb.meta.RDBColumnMetaData;
import org.hswebframework.ezorm.rdb.supports.commons.SimpleSQL;
import org.hswebframework.utils.StringUtils;
import org.hswebframework.ezorm.rdb.executor.BindSQL;
import org.hswebframework.ezorm.rdb.meta.RDBTableMetaData;
import org.hswebframework.ezorm.rdb.executor.SQL;
import org.hswebframework.ezorm.rdb.render.SqlAppender;
import org.hswebframework.ezorm.rdb.render.SqlRender;

import java.util.*;
import java.util.stream.Collectors;

/**
 * sqlServer 表结构修改sql渲染器,用于渲染sqlServer修改表的sql
 */
public class SqlServerMetaAlterRender extends CommentSupportRender implements SqlRender<Boolean> {
    @Override
    public SQL render(RDBTableMetaData table, Boolean executeRemove) {
        RDBTableMetaData old = table.getDatabaseMetaData().getTableMetaData(table.getName());

        if (old == null) throw new UnsupportedOperationException("旧表不存在!");
        List<RDBColumnMetaData> changedField = new ArrayList<>();
        List<RDBColumnMetaData> addedField = new ArrayList<>();
        List<RDBColumnMetaData> deletedField = new ArrayList<>();

        RDBTableMetaData oldMeta = old;
        if (executeRemove)
            oldMeta.getColumns().forEach(oldField -> {
                RDBColumnMetaData newMeta = table.findColumn(oldField.getName());
                if (newMeta == null) {
                    newMeta = table.getColumns().stream()
                            .filter(columnMetaData -> oldField.getName().equals(columnMetaData.getProperty("old-name").getValue()))
                            .findFirst().orElse(null);
                }
                if (newMeta == null || !newMeta.getName().equals(oldField.getName())) {
                    //删除的字段
                    deletedField.add(oldField);
                }
            });
        table.getColumns().forEach(newField -> {
            String oldName = newField.getProperty("old-name").getValue();
            if (oldName == null) oldName = newField.getName();
            RDBColumnMetaData oldField = oldMeta.findColumn(oldName);
            if (oldField == null) {
                //增加的字段
                addedField.add(newField);
            } else {
                if (!newField.getName().equals(oldField.getName()) ||
                        !newField.getDataType().equals(oldField.getDataType())
                        || !newField.getComment().equals(oldField.getComment())
                        || oldField.isNotNull() != newField.isNotNull()) {
                    changedField.add(newField);
                }
            }
        });
        LinkedList<BindSQL> bind = new LinkedList<>();
        List<SQL> comments = new ArrayList<>();
        String newTableComment = table.getComment();
        String oldTableComment = old.getComment();
        if (newTableComment == null) newTableComment = "";
        if (oldTableComment == null) oldTableComment = "";
        if (!newTableComment.equals(oldTableComment)) {
            comments.add(buildTableCommentSql(table.getName(), table.getComment()));
        }
        if (addedField.isEmpty() && changedField.isEmpty() && deletedField.isEmpty() && comments.isEmpty()) {
            return new EmptySQL();
        }
        addedField.forEach(column -> {
            SqlAppender append = new SqlAppender();
            append.add("ALTER TABLE ", table.getName(), " ADD [", column.getName(), "] ", column.getDataType());
            if (column.isNotNull()) {
                append.add(" NOT NULL");
            }
            if (column.isPrimaryKey()) {
                append.add(" PRIMARY KEY ");
            }
            if (StringUtils.isNullOrEmpty(column.getComment())) {
                comments.add(buildDropColumnCommentSql(table.getName(), column.getName(), column.getAlias()));
            } else {
                comments.add(buildColumnCommentSql(table.getName(), column.getName(), column.getComment()));
            }
            SimpleSQL simpleSQL = new SimpleSQL(append.toString(), column);
            BindSQL bindSQL = new BindSQL();
            bindSQL.setSql(simpleSQL);
            bindSQL.setToField(column.getName());
            bind.add(bindSQL);
        });
        changedField.forEach(column -> {
            String oldName = column.getProperty("old-name").getValue();
            if (oldName == null) oldName = column.getName();
            RDBColumnMetaData oldColumn = oldMeta.findColumn(oldName);

            if (!oldName.equals(column.getName())) {
                String sql = "EXEC sp_rename #{old}, #{name}, 'COLUMN'";
                Map<String, Object> param = new HashMap<>();
                param.put("old", oldName);
                param.put("name", column.getName());
                BindSQL bindSQL = new BindSQL();
                bindSQL.setSql(new SimpleSQL(sql, param));
                bind.add(bindSQL);
                table.renameColumn(oldName, column.getName());
            }
            if (!oldColumn.getDataType().equals(column.getDataType())
                    || oldColumn.isNotNull() != column.isNotNull()) {
                SqlAppender append = new SqlAppender();
                append.add("ALTER TABLE ", table.getName(), " ALTER COLUMN [", column.getName(), "] ", column.getDataType());
                if (oldColumn.isNotNull() != column.isNotNull()) {
                    if (column.isNotNull()) {
                        append.add(" NOT NULL");
                    } else {
                        append.add(" NULL");
                    }
                }
                SimpleSQL simpleSQL = new SimpleSQL(append.toString(), column);
                BindSQL bindSQL = new BindSQL();
                bindSQL.setSql(simpleSQL);
                bindSQL.setToField(column.getName());
                bind.add(bindSQL);
            }
            String nc = column.getComment();
            String oc = oldColumn.getComment();
            if (nc == null) nc = "";
            if (oc == null) oc = "";
            if (nc.equals(oc)) return;

            if (StringUtils.isNullOrEmpty(nc)) {
                comments.add(buildDropColumnCommentSql(table.getName(), column.getName(), column.getAlias()));
            } else {
                comments.add(buildColumnCommentSql(table.getName(), column.getName(), nc));
            }
        });
        deletedField.forEach(column -> {
            String dropSql = String.format("ALTER TABLE %s DROP COLUMN [%s]", table.getName(), column.getName());
            SimpleSQL simpleSQL = new SimpleSQL(dropSql, column);
            BindSQL bindSQL = new BindSQL();
            bindSQL.setSql(simpleSQL);
            bindSQL.setToField(column.getName());
            bind.add(bindSQL);
        });
        LinkedList<BindSQL> commentSql = new LinkedList<>(comments.stream().map(s -> {
            BindSQL binSql = new BindSQL();
            binSql.setSql(s);
            return binSql;
        }).collect(Collectors.toList()));

        SQL sql = null;
        if (bind.isEmpty()) {
            return new EmptySQL();
        }
        bind.addAll(commentSql);
        if (!bind.isEmpty()) {
            sql = bind.get(0).getSql();
            bind.removeFirst();
        }
        if (sql != null && !bind.isEmpty())
            ((SimpleSQL) sql).setBindSQLs(bind);
        return sql;
    }

    @Override
    protected boolean isDropCommentBefore() {
        return true;
    }
}
