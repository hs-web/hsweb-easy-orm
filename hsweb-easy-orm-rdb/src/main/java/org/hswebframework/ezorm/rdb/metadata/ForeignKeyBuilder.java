package org.hswebframework.ezorm.rdb.metadata;

import lombok.*;
import org.hswebframework.ezorm.core.param.Term;

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

    private String sourceColumn;

    private String target;

    private String targetColumn;

    private List<Term> terms;
}
