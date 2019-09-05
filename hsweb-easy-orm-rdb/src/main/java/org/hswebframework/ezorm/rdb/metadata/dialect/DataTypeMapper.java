package org.hswebframework.ezorm.rdb.metadata.dialect;

import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;

 public interface DataTypeMapper {
    String getDataType(RDBColumnMetadata columnMetaData);
}
