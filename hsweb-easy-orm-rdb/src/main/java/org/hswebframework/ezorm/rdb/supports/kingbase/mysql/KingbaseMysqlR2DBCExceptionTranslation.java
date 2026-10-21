package org.hswebframework.ezorm.rdb.supports.kingbase.mysql;

import io.r2dbc.spi.R2dbcException;
import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.exception.DuplicateKeyException;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.operator.ExceptionTranslation;

import java.util.Collections;
import java.util.Locale;

/**
 * Converts duplicate-key errors emitted by the Kingbase R2DBC driver.
 */
@AllArgsConstructor(staticName = "of")
public class KingbaseMysqlR2DBCExceptionTranslation implements ExceptionTranslation {

    @SuppressWarnings("unused")
    private final RDBSchemaMetadata schema;

    @Override
    public Throwable translate(Throwable e) {
        if (e instanceof R2dbcException) {
            R2dbcException exception = (R2dbcException) e;
            if (exception.getErrorCode() == 1062
                || exception.getErrorCode() == 1022
                || "23505".equals(exception.getSqlState())
                || isDuplicateKeyMessage(exception.getMessage())) {
                return new DuplicateKeyException(false, Collections.emptyList(), e);
            }
        }
        return e;
    }

    private boolean isDuplicateKeyMessage(String message) {
        if (message == null) {
            return false;
        }
        String lowerCase = message.toLowerCase(Locale.ROOT);
        return lowerCase.contains("duplicate key")
            || lowerCase.contains("duplicated key")
            || lowerCase.contains("unique constraint");
    }
}
