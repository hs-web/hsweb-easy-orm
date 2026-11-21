package org.hswebframework.ezorm.rdb.supports.kingbaseES.mssql;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.exception.DuplicateKeyException;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.operator.ExceptionTranslation;

import java.sql.SQLException;
import java.util.Collections;

/** Accepts Kingbase SQLSTATE and SQL Server compatible duplicate-key codes. */
@AllArgsConstructor(staticName = "of")
public class KingbaseSqlServerJDBCExceptionTranslation implements ExceptionTranslation {

    private final RDBSchemaMetadata schema;

    @Override
    public Throwable translate(Throwable error) {
        if (error instanceof SQLException) {
            SQLException exception = (SQLException) error;
            if ("23505".equals(exception.getSQLState())
                || exception.getErrorCode() == 2601 || exception.getErrorCode() == 2627) {
                return new DuplicateKeyException(false, Collections.emptyList(), error);
            }
        }
        return error;
    }
}
