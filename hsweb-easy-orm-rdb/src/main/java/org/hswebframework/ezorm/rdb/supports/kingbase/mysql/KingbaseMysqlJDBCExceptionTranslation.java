package org.hswebframework.ezorm.rdb.supports.kingbase.mysql;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.exception.DuplicateKeyException;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.operator.ExceptionTranslation;

import java.sql.SQLException;
import java.util.Collections;
import java.util.Locale;

/**
 * Converts duplicate-key errors emitted by the Kingbase JDBC driver.
 */
@AllArgsConstructor(staticName = "of")
public class KingbaseMysqlJDBCExceptionTranslation implements ExceptionTranslation {

    @SuppressWarnings("unused")
    private final RDBSchemaMetadata schema;

    @Override
    public Throwable translate(Throwable e) {
        if (e instanceof SQLException) {
            SQLException exception = (SQLException) e;
            if (exception.getErrorCode() == 1062
                || exception.getErrorCode() == 1022
                || "23505".equals(exception.getSQLState())
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
