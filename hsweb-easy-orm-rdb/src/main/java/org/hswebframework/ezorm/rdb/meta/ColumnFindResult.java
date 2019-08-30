package org.hswebframework.ezorm.rdb.meta;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class ColumnFindResult {
    private String ownerAlias;

    private String owner;

    private RDBColumnMetadata column;


}
