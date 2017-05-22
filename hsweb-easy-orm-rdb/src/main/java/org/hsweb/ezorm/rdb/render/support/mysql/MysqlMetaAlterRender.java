package org.hsweb.ezorm.rdb.render.support.mysql;

import org.hsweb.ezorm.rdb.executor.EmptySQL;
import org.hsweb.ezorm.rdb.executor.SQL;
import org.hsweb.ezorm.rdb.meta.RDBColumnMetaData;
import org.hsweb.ezorm.rdb.meta.RDBDatabaseMetaData;
import org.hsweb.ezorm.rdb.meta.RDBTableMetaData;
import org.hsweb.ezorm.rdb.render.SqlAppender;
import org.hsweb.ezorm.rdb.render.SqlRender;
import org.hsweb.ezorm.rdb.render.support.simple.SimpleSQL;
import org.hswebframwork.utils.StringUtils;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.NoSuchElementException;


/**
 * Created by zhouhao on 16-6-5.
 */
public class MysqlMetaAlterRender implements SqlRender<Boolean> {

    private RDBDatabaseMetaData databaseMetaData;

    public MysqlMetaAlterRender(RDBDatabaseMetaData databaseMetaData) {
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
        List<String> comments = new ArrayList<>();
        String newTableComment = metaData.getComment();
        String oldTableComment = old.getComment();
        if (newTableComment == null) newTableComment = "";
        if (oldTableComment == null) oldTableComment = "";
        if (!newTableComment.equals(oldTableComment)) {
            comments.add(String.format("comment='%s'", newTableComment));
        }
        if (addedField.isEmpty() && changedField.isEmpty() && deletedField.isEmpty() && comments.isEmpty()) {
            return new EmptySQL();
        }
        List<SqlAppender> addSql = new LinkedList<>();
        List<SqlAppender> changedSql = new LinkedList<>();
        List<SqlAppender> deleteSql = new LinkedList<>();

        addedField.forEach(field -> {
            SqlAppender append = new SqlAppender();
            append.add("add column ", field.getName(), " ", field.getDataType());
            if (!StringUtils.isNullOrEmpty(field.getProperty("default-value").getValue())) {
                append.add(" default '", field.getProperty("default-value").getValue(), "'");
            }
            if (field.isNotNull()) {
                append.add(" not null ");
            } else {
                append.add(" null ");
            }
            if (!StringUtils.isNullOrEmpty(field.getComment())) {
                append.add(" comment '", field.getComment(), "'");
            }
            addSql.add(append);
        });
        changedField.forEach(field -> {
            String oldName = field.getProperty("old-name").getValue();
            if (oldName == null) oldName = field.getName();
            SqlAppender append = new SqlAppender();
            append.addSpc("change", oldName, field.getName(), field.getDataType());
            if (!StringUtils.isNullOrEmpty(field.getProperty("default-value").getValue())) {
                append.add("default '", field.getProperty("default-value").getValue(), "'");
            }
            if (field.isNotNull()) {
                append.add(" not null ");
            } else {
                append.add(" null ");
            }
            if (!StringUtils.isNullOrEmpty(field.getComment())) {
                append.add(" comment '", field.getComment(), "'");
            }
            changedSql.add(append);
        });
        deletedField.forEach(field -> deleteSql.add(new SqlAppender().add("drop column ", field.getName())));
        List<SqlAppender> allSql = new LinkedList<>();
        allSql.add(new SqlAppender().addSpc(String.format("alter table `%s`", metaData.getName())));
        allSql.addAll(deleteSql);
        allSql.addAll(addSql);
        allSql.addAll(changedSql);
        if (!comments.isEmpty()) {
            allSql.add(new SqlAppender().add(comments.toArray()));
        }
        StringBuilder builder = new StringBuilder();
        for (int i = 0; i < allSql.size(); i++) {
            SqlAppender sql = allSql.get(i);
            if (i > 1) {
                builder.append(",");
            }
            builder.append(sql.toString());
        }
        return new SimpleSQL(builder.toString());
    }
}
