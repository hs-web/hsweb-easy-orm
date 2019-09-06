package org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl;

import lombok.*;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class AlterRequest {

    private RDBTableMetadata newTable;

    private RDBTableMetadata oldTable;

    private boolean allowDrop;

    private boolean forceAlterDataType;
}
