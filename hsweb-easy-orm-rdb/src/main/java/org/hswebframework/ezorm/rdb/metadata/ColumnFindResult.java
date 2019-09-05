package org.hswebframework.ezorm.rdb.metadata;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class ColumnFindResult {
    private String ownerAlias;

    private String owner;

    private RDBColumnMetadata column;


}
