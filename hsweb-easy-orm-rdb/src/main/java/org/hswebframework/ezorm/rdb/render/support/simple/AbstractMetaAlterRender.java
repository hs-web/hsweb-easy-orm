package org.hswebframework.ezorm.rdb.render.support.simple;

import org.hswebframework.ezorm.rdb.executor.BindSQL;
import org.hswebframework.ezorm.rdb.executor.EmptySQL;
import org.hswebframework.ezorm.rdb.executor.SQL;
import org.hswebframework.ezorm.rdb.meta.RDBColumnMetaData;
import org.hswebframework.ezorm.rdb.meta.RDBDatabaseMetaData;
import org.hswebframework.ezorm.rdb.meta.RDBTableMetaData;
import org.hswebframework.ezorm.rdb.render.SqlAppender;
import org.hswebframework.ezorm.rdb.render.SqlRender;

import java.util.*;
import java.util.stream.Collectors;

/**
 * @author zhouhao
 * @since 3.0
 */
public abstract class AbstractMetaAlterRender implements SqlRender<Boolean> {

    protected RDBDatabaseMetaData databaseMetaData;

    public AbstractMetaAlterRender(RDBDatabaseMetaData databaseMetaData) {
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
        if (executeRemove) {
            oldMeta.getColumns().forEach(oldField -> {
                RDBColumnMetaData newMeta = metaData.findColumn(oldField.getName());
                if (newMeta == null) {
                    newMeta = metaData.getColumns().stream()
                            .filter(fieldMetaData -> oldField.getName().equals(fieldMetaData.getProperty("old-name").getValue()))
                            .findFirst().orElse(null);
                }
                if (newMeta == null || !newMeta.getName().equals(oldField.getName())) {
                    //删除的字段
                    deletedField.add(oldField);
                }
            });
        }
        metaData.getColumns().forEach(newField -> {
            String oldName = newField.getProperty("old-name").getValue();
            if (oldName == null) oldName = newField.getName();
            RDBColumnMetaData oldField = oldMeta.findColumn(oldName);
            if (oldField == null) {
                //增加的字段
                addedField.add(newField);
            } else {
                if (!newField.getName().equals(oldField.getName()) ||
                        //可能改变了精度?
                        (newField.getJdbcType() == oldField.getJdbcType() &&
                                !newField.getDataType().equals(oldField.getDataType()))) {
                    changedField.add(newField);
                }
            }
        });
        if (addedField.isEmpty() && changedField.isEmpty() && deletedField.isEmpty()) {
            return new EmptySQL();
        }
        List<SqlAppender> addSql = addedField
                .stream()
                .map(this::buildAdd)
                .flatMap(Collection::stream)
                .collect(Collectors.toList());

        List<SqlAppender> changedSql = changedField
                .stream()
                .map(this::buildAlter)
                .flatMap(Collection::stream)
                .collect(Collectors.toList());

        List<SqlAppender> deleteSql = deletedField
                .stream()
                .map(this::buildDrop)
                .flatMap(Collection::stream)
                .collect(Collectors.toList());
        List<SqlAppender> all = new ArrayList<>();
        all.addAll(addSql);
        all.addAll(changedSql);
        all.addAll(deleteSql);

        LinkedList<SimpleSQL> sqlList = new LinkedList<>(merge(all).stream()
                .map(SqlAppender::toString)
                .map(SimpleSQL::new)
                .collect(Collectors.toList()));
        if (sqlList.isEmpty()) {
            return new EmptySQL();
        }
        SimpleSQL mainSql = sqlList.get(0);

        sqlList.removeFirst();

        mainSql.setBindSQLs(sqlList.stream().map(BindSQL::new).collect(Collectors.toList()));

        return mainSql;
    }

    protected List<SqlAppender> merge(List<SqlAppender> sql) {
        return sql;
    }

    protected List<SqlAppender> buildAdd(RDBColumnMetaData column) {
        SqlAppender append = new SqlAppender();
        append.add("alter table ",
                column.getTableMetaData().getFullName(),
                " add ",
                column.getName(),
                " ");
        if (column.getColumnDefinition() != null) {
            append.add(column.getColumnDefinition());
        } else {
            append.add(column.getDataType());
            if (column.isNotNull() || column.isPrimaryKey()) {
                append.add(" not null");
            }
            if (column.getComment() != null) {
                append.add(" comment '", column.getComment(), "'");
            }
        }
        return new ArrayList<>(Collections.singletonList(append));
    }

    protected List<SqlAppender> buildAlter(RDBColumnMetaData column) {
        SqlAppender append = new SqlAppender();
        append.add("alter table ",
                column.getTableMetaData().getFullName(),
                " modify ",
                column.getName(),
                " ");
        if (column.getColumnDefinition() != null) {
            append.add(column.getColumnDefinition());
        } else {
            append.add(column.getDataType());
            if (column.isNotNull() || column.isPrimaryKey()) {
                append.add(" not null");
            }
            if (column.getComment() != null) {
                append.add(" comment '", column.getComment(), "'");
            }
        }
        return new ArrayList<>(Collections.singletonList(append));
    }

    protected List<SqlAppender> buildDrop(RDBColumnMetaData column) {
        String dropSql = String.format("alter table %s drop column %s", column.getTableMetaData().getFullName(), column.getName());
        return new ArrayList<>(Collections.singletonList(new SqlAppender(dropSql)));
    }


}
