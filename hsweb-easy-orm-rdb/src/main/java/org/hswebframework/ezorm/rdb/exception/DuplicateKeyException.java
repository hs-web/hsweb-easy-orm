package org.hswebframework.ezorm.rdb.exception;

import lombok.Getter;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;

import java.util.List;

@Getter
public class DuplicateKeyException extends RuntimeException {

    private final boolean primaryKey;

    private final List<RDBColumnMetadata> columns;

    public DuplicateKeyException(boolean primaryKey,
                                 List<RDBColumnMetadata> columns,
                                 Throwable cause) {
        super(cause);
        this.primaryKey = primaryKey;
        this.columns = columns;

    }

}
