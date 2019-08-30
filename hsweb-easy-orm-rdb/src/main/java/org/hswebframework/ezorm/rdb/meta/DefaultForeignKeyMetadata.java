package org.hswebframework.ezorm.rdb.meta;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.core.meta.SchemaMetadata;
import org.hswebframework.ezorm.core.param.Term;

import java.util.List;

@Getter
@Setter
public class DefaultForeignKeyMetadata implements ForeignKeyMetadata {

    private String name;

    private String alias;

    private boolean logical;

    private boolean toMany;

    private TableOrViewMetadata source;

    private TableOrViewMetadata target;

    private RDBColumnMetadata sourceColumn;

    private RDBColumnMetadata targetColumn;

    private List<Term> terms;


}
