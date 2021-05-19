package org.hswebframework.ezorm.rdb.supports.mysql;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.exception.DuplicateKeyException;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.operator.ExceptionTranslation;

import java.sql.SQLException;
import java.util.Collections;

@AllArgsConstructor(staticName = "of")
public class MysqlJDBCExceptionTranslation implements ExceptionTranslation {

    private final RDBSchemaMetadata schema;

    @Override
    public Throwable translate(Throwable e) {
        if (e instanceof SQLException) {
            SQLException exception = ((SQLException) e);
            if (exception.getErrorCode() == 1062 || exception.getErrorCode() == 1022) {
                throw new DuplicateKeyException(true, Collections.emptyList(), e);
            }
        }
        return e;
    }
}
