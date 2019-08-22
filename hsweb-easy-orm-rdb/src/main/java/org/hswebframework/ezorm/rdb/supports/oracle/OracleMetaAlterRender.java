package org.hswebframework.ezorm.rdb.supports.oracle;

import org.hswebframework.ezorm.rdb.meta.RDBColumnMetaData;
import org.hswebframework.ezorm.rdb.meta.RDBDatabaseMetaData;
import org.hswebframework.ezorm.rdb.render.SqlAppender;
import org.hswebframework.ezorm.rdb.supports.commons.AbstractMetaAlterRender;

import java.util.ArrayList;
import java.util.List;


/**
 * oracle数据库表结构修改sql渲染器
 *
 * @author zhouhao
 * @see 1.0
 */
public class OracleMetaAlterRender extends AbstractMetaAlterRender {

    public OracleMetaAlterRender(RDBDatabaseMetaData databaseMetaData) {
        super(databaseMetaData);
    }

    @Override
    protected List<SqlAppender> buildAdd(RDBColumnMetaData column) {
        SqlAppender alter = new SqlAppender();
        SqlAppender comments = new SqlAppender();
        List<SqlAppender> all = new ArrayList<>();
        alter.add("ALTER TABLE ",
                column.getTableMetaData().getFullName(),
                " ADD ",
                column.getName(),
                " ");
        if (column.getColumnDefinition() != null) {
            alter.add(column.getColumnDefinition());
        } else {
            alter.add(column.getDataType());
            if (column.isNotNull() || column.isPrimaryKey()) {
                alter.add(" NOT NULL");
            }
            if (column.getComment() != null) {
                comments.add(String.format("COMMENT ON COLUMN %s.\"%s\" is '%s'", column.getTableMetaData().getFullName(), column.getName().toUpperCase(), column.getComment()));
            }
        }
        all.add(alter);
        all.add(comments);
        return all;
    }

    @Override
    protected List<SqlAppender> buildAlter(RDBColumnMetaData column) {
        SqlAppender alter = new SqlAppender();
        SqlAppender comments = new SqlAppender();
        List<SqlAppender> all = new ArrayList<>();
        alter.add("ALTER TABLE ",
                column.getTableMetaData().getName(),
                " MODIFY ",
                column.getName(),
                " ");
        if (column.getColumnDefinition() != null) {
            alter.add(column.getColumnDefinition());
        } else {
            alter.add(column.getDataType());
            if (column.isNotNull() || column.isPrimaryKey()) {
                alter.add(" NOT NULL");
            }
            if (column.getComment() != null) {
                comments.add(String.format("COMMENT ON COLUMN %s.\"%s\" is '%s'", column.getTableMetaData().getFullName(), column.getName().toUpperCase(), column.getComment()));
            }
        }
        all.add(alter);
        all.add(comments);
        return all;
    }

}
