package org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.rdb.metadata.RDBIndexMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;

@Getter
@Setter
@AllArgsConstructor(staticName = "of")
public class DropIndexParameter {
    private RDBTableMetadata table;

    private RDBIndexMetadata index;
}
