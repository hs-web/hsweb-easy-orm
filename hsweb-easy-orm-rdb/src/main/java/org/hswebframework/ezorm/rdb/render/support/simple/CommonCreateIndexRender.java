package org.hswebframework.ezorm.rdb.render.support.simple;


import org.hswebframework.ezorm.rdb.executor.SQL;
import org.hswebframework.ezorm.rdb.meta.RDBIndexMetaData;
import org.hswebframework.ezorm.rdb.meta.RDBTableMetaData;
import org.hswebframework.ezorm.rdb.render.SqlAppender;

import java.util.List;
import java.util.stream.Collectors;

/**
 * @author zhouhao
 * @since 1.0.0
 */
public class CommonCreateIndexRender {

    public static List<SQL> buildCreateIndexSql(RDBTableMetaData table) {
        return table.getIndexes()
                .stream()
                .map(index -> buildIndex(table.getFullName(), index, table))
                .collect(Collectors.toList());
    }

    public static SQL buildIndex(String table, RDBIndexMetaData index, RDBTableMetaData tableMeta) {
        SqlAppender appender = new SqlAppender();
        appender.addSpc("create",
                index.isUnique() ? "unique" : "",
                "index",
                index.getIndexName(),
                "on", table, "("
        );

        for (RDBIndexMetaData.IndexColumn indexColumn : index.getColumnName()) {
            appender.add(tableMeta.getDatabaseMetaData()
                            .getDialect()
                            .buildColumnName(null, indexColumn.getColumn()),
                    " ",
                    (indexColumn.getSort() == null ? "" : indexColumn.getSort()));
            appender.add(",");
        }
        appender.removeLast();
        appender.add(")");

        return new SimpleSQL(appender.toString());

    }

}
