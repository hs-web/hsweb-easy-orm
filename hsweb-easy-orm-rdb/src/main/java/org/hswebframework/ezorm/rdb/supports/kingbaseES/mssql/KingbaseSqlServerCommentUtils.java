package org.hswebframework.ezorm.rdb.supports.kingbaseES.mssql;

import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;

/** Single-statement comments supported by the Kingbase extended-query protocol. */
final class KingbaseSqlServerCommentUtils {

    static SqlRequest table(RDBTableMetadata table) {
        return SqlRequests.of("comment on table " + table.getFullName() + " is " + literal(table.getComment()));
    }

    static SqlRequest column(RDBColumnMetadata column) {
        return SqlRequests.of("comment on column " + column.getFullTableName() + " is " + literal(column.getComment()));
    }

    private static String literal(String comment) {
        return comment == null ? "null" : "'" + comment.replace("'", "''") + "'";
    }
}
