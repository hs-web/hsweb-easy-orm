package org.hswebframework.ezorm.rdb.supports.kingbaseES.mssql;

import io.r2dbc.spi.R2dbcException;
import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.exception.DuplicateKeyException;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.operator.ExceptionTranslation;

import java.util.Collections;
import java.util.concurrent.CompletionException;
import java.util.concurrent.ExecutionException;

/** Accepts Kingbase SQLSTATE and SQL Server compatible duplicate-key codes. */
@AllArgsConstructor(staticName = "of")
public class KingbaseSqlServerR2DBCExceptionTranslation implements ExceptionTranslation {

    private final RDBSchemaMetadata schema;

    @Override
    public Throwable translate(Throwable error) {
        if ((error instanceof ExecutionException || error instanceof CompletionException) && error.getCause() != null) {
            Throwable translated = translate(error.getCause());
            return translated == error.getCause() ? error : translated;
        }
        if (error instanceof R2dbcException) {
            R2dbcException exception = (R2dbcException) error;
            if ("23505".equals(exception.getSqlState())
                || exception.getErrorCode() == 2601 || exception.getErrorCode() == 2627) {
                return new DuplicateKeyException(false, Collections.emptyList(), error);
            }
        }
        return error;
    }
}
