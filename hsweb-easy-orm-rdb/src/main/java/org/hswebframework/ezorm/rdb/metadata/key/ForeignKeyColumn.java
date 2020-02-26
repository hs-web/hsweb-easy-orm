package org.hswebframework.ezorm.rdb.metadata.key;

import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;

public interface ForeignKeyColumn {
    RDBColumnMetadata getTargetColumn();

    RDBColumnMetadata getSourceColumn();
}
