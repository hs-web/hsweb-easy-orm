package org.hswebframework.ezorm.rdb.metadata;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.operator.dml.JoinType;

import java.util.List;

@Getter
@Setter
public class DefaultForeignKeyMetadata implements ForeignKeyMetadata {

    private String name;

    private String alias;

    private boolean logical;

    private boolean toMany;

    private boolean autoJoin;

    private TableOrViewMetadata source;

    private TableOrViewMetadata target;

    private RDBColumnMetadata sourceColumn;

    private RDBColumnMetadata targetColumn;

    private JoinType joinType = JoinType.left;

    private List<Term> terms;

    private List<ForeignKeyMetadata> preForeignKey;

    public String getAlias() {
        if (alias == null) {
            alias = name;
        }
        return alias;
    }
}
