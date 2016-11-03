package org.hsweb.ezorm.rdb.render.support.h2;

import org.hsweb.commons.StringUtils;
import org.hsweb.ezorm.rdb.executor.BindSQL;
import org.hsweb.ezorm.rdb.meta.RDBTableMetaData;
import org.hsweb.ezorm.rdb.executor.EmptySQL;
import org.hsweb.ezorm.rdb.executor.SQL;
import org.hsweb.ezorm.rdb.meta.RDBDatabaseMetaData;
import org.hsweb.ezorm.rdb.meta.RDBColumnMetaData;
import org.hsweb.ezorm.rdb.render.SqlAppender;
import org.hsweb.ezorm.rdb.render.SqlRender;
import org.hsweb.ezorm.rdb.render.support.simple.SimpleSQL;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.stream.Collectors;


/**
 * Created by zhouhao on 16-6-5.
 */
public class H2MetaAlterRender implements SqlRender<Boolean> {

    private RDBDatabaseMetaData databaseMetaData;

    public H2MetaAlterRender(RDBDatabaseMetaData databaseMetaData) {
        this.databaseMetaData = databaseMetaData;
    }

    @Override
    public SQL render(RDBTableMetaData metaData, Boolean executeRemove) {
        RDBTableMetaData old = databaseMetaData.getTableMetaData(metaData.getName());
        if (old == null) throw new UnsupportedOperationException("旧表不存在!");
        List<RDBColumnMetaData> changedField = new ArrayList<>();
        List<RDBColumnMetaData> addedField = new ArrayList<>();
        List<RDBColumnMetaData> deletedField = new ArrayList<>();

        RDBTableMetaData oldMeta = old;
        if (executeRemove)
            oldMeta.getColumns().forEach(oldField -> {
                RDBColumnMetaData newMeta = metaData.findColumn(oldField.getName());
                if (newMeta == null) {
                    try {
                        newMeta = metaData.getColumns().stream()
                                .filter(fieldMetaData -> oldField.getName().equals(fieldMetaData.getProperty("old-name").getValue()))
                                .findFirst().get();
                    } catch (NoSuchElementException e) {
                    }
                }
                if (newMeta == null) {
                    //删除的字段
                    deletedField.add(oldField);
                }
            });
        metaData.getColumns().forEach(newField -> {
            String oldName = newField.getProperty("old-name", newField.getName()).getValue();
            RDBColumnMetaData oldField = oldMeta.findColumn(oldName);
            if (oldField == null) {
                //增加的字段
                addedField.add(newField);
            } else {
                if (!newField.getName().equals(oldField.getName()) ||
                        !newField.getDataType().equals(oldField.getDataType())
                        || !newField.getComment().equals(oldField.getComment())
                        || oldField.getProperty("not-null", false).getValue() != newField.getProperty("not-null", false).getValue()) {
                    changedField.add(newField);
                }
            }
        });
        LinkedList<BindSQL> bind = new LinkedList<>();
        List<String> comments = new ArrayList<>();
        String newTableComment = metaData.getComment();
        String oldTableComment = old.getComment();
        if (newTableComment == null) newTableComment = "";
        if (oldTableComment == null) oldTableComment = "";
        if (!newTableComment.equals(oldTableComment)) {
            comments.add(String.format("COMMENT ON TABLE %s IS '%s'", metaData.getName(), metaData.getComment()));
        }
        if (addedField.isEmpty() && changedField.isEmpty() && deletedField.isEmpty() && comments.isEmpty()) {
            return new EmptySQL();
        }
        addedField.forEach(field -> {
            SqlAppender append = new SqlAppender();
            append.add("ALTER TABLE ", metaData.getName(), " ADD ", field.getName(), " ", field.getDataType());
            if (field.isNotNull()) {
                append.add(" not null");
            }
            if (StringUtils.isNullOrEmpty(field.getComment())) {
//                comments.add(String.format("COMMENT ON COLUMN %s.%s is '%s'", metaData.getName(), field.getName(), field.getAlias()));
            } else {
                comments.add(String.format("COMMENT ON COLUMN %s.%s is '%s'", metaData.getName(), field.getName(), field.getComment()));
            }
            SimpleSQL simpleSQL = new SimpleSQL(append.toString(), field);
            BindSQL bindSQL = new BindSQL();
            bindSQL.setSql(simpleSQL);
            bindSQL.setToField(field.getName());
            bind.add(bindSQL);
        });
        changedField.forEach(field -> {
            String oldName = field.getProperty("old-name").getValue();
            if (oldName == null) oldName = field.getName();
            RDBColumnMetaData oldField = oldMeta.findColumn(oldName);
            if (!oldName.equals(field.getName())) {
                SqlAppender renameSql = new SqlAppender();
                renameSql.add("ALTER TABLE ", metaData.getName(), " ALTER COLUMN ", oldName, " RENAME TO ", field.getName());
                BindSQL bindSQL = new BindSQL();
                bindSQL.setSql(new SimpleSQL(renameSql.toString()));
                bind.add(bindSQL);
                metaData.renameColumn(oldName, field.getName());
            }
            if (!oldField.getDataType().equals(field.getDataType())
                    || oldField.isNotNull() != field.isNotNull()) {
                SqlAppender append = new SqlAppender();
                append.add("ALTER TABLE ", metaData.getName(), " MODIFY ", field.getName(), " ", field.getDataType());
                if (field.isNotNull()) {
                    append.add(" NOT NULL");
                } else {
                    append.add(" NULL");
                }
                if (field.isPrimaryKey()) append.add(" primary key");

                SimpleSQL simpleSQL = new SimpleSQL(append.toString(), field);
                BindSQL bindSQL = new BindSQL();
                bindSQL.setSql(simpleSQL);
                bindSQL.setToField(field.getName());
                bind.add(bindSQL);
            }
            String nc = field.getComment();
            String oc = oldField.getComment();
            if (nc == null) nc = "";
            if (oc == null) oc = "";
            if (nc.equals(oc)) return;
            if (StringUtils.isNullOrEmpty(nc)) {
                comments.add(String.format("COMMENT ON COLUMN %s.%s IS '新建字段:%s'", metaData.getName(), field.getName(), field.getAlias()));
            } else {
                comments.add(String.format("COMMENT ON COLUMN %s.%s IS '%s'", metaData.getName(), field.getName(), nc));
            }
        });
        deletedField.forEach(field -> {
            String dropSql = String.format("ALTER TABLE %s DROP COLUMN %s", metaData.getName(), field.getName());
            SimpleSQL simpleSQL = new SimpleSQL(dropSql, field);
            BindSQL bindSQL = new BindSQL();
            bindSQL.setSql(simpleSQL);
            bindSQL.setToField(field.getName());
            bind.add(bindSQL);
        });
        LinkedList<BindSQL> commentSql = new LinkedList<>(comments.stream().map(s -> {
            BindSQL binSql = new BindSQL();
            binSql.setSql(new SimpleSQL(s, s));
            return binSql;
        }).collect(Collectors.toList()));

        SQL sql = null;
        bind.addAll(commentSql);
        if (!bind.isEmpty()) {
            sql = bind.get(0).getSql();
            bind.removeFirst();
        }
        if (!bind.isEmpty())
            ((SimpleSQL) sql).setBindSQLs(bind);
        return sql;
    }

}
