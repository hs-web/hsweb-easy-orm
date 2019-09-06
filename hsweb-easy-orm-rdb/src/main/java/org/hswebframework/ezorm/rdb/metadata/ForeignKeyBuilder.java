package org.hswebframework.ezorm.rdb.metadata;

import lombok.*;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.operator.dml.JoinType;

import java.util.List;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ForeignKeyBuilder {
    private String name;

    private String alias;

    private boolean toMany;

    private boolean autoJoin;

    private String sourceColumn;

    private String target;

    private String targetColumn;

    private List<Term> terms;

    private JoinType joinType;

    private List<ForeignKeyMetadata> preForeignKey;


}
