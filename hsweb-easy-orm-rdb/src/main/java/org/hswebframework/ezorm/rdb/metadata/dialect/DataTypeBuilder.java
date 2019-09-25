package org.hswebframework.ezorm.rdb.metadata.dialect;

import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;

 public interface DataTypeBuilder {
    String createColumnDataType(RDBColumnMetadata columnMetaData);
}
