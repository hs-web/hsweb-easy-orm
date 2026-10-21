package org.hswebframework.ezorm.rdb.supports.kingbase.oracle;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.exception.DuplicateKeyException;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.operator.ExceptionTranslation;

import java.sql.SQLException;
import java.util.Collections;
import java.util.Locale;

@AllArgsConstructor(staticName = "of")
public class KingbaseOracleJDBCExceptionTranslation implements ExceptionTranslation {

    @SuppressWarnings("unused")
    private final RDBSchemaMetadata schema;

    @Override
    public Throwable translate(Throwable error) {
        if (error instanceof SQLException) {
            SQLException exception = (SQLException) error;
            if (isDuplicateKey(exception.getErrorCode(), exception.getSQLState(), exception.getMessage())) {
                return new DuplicateKeyException(false, Collections.emptyList(), error);
            }
        }
        return error;
    }

    static boolean isDuplicateKey(int errorCode, String sqlState, String message) {
        if ("23505".equals(sqlState) || "23000".equals(sqlState) || errorCode == 1062 || errorCode == 1022) {
            return true;
        }
        if (message == null) {
            return false;
        }
        String lowerCase = message.toLowerCase(Locale.ROOT);
        return lowerCase.contains("duplicate key")
            || lowerCase.contains("duplicated key")
            || lowerCase.contains("unique constraint");
    }
}
