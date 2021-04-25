package org.hswebframework.ezorm.rdb.operator.builder.fragments.term;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.TermFragmentBuilder;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

@AllArgsConstructor
public abstract class AbstractTermFragmentBuilder implements TermFragmentBuilder {

    @Getter
    @Setter
    private String termType;

    @Getter
    @Setter
    private String name;

    @SuppressWarnings("all")
    protected List<Object> convertList(RDBColumnMetadata column, Term term) {
        Object value = term.getValue();
        if (value == null) {
            return Collections.emptyList();
        }
        if (value instanceof String) {
            value = ((String) value).split(",");
        }

        if (value instanceof Object[]) {
            value = Arrays.asList(((Object[]) value));
        }

        if (value instanceof Collection) {
            return ((Collection<Object>) value).stream()
                    .map(val -> this.convertValue(column, val))
                    .collect(Collectors.toList());
        }

        return Arrays.asList(this.convertValue(column, value));
    }

    private Object convertValue(RDBColumnMetadata column, Object val) {
        if(column==null){
            return val;
        }
        return column.encode(val);
    }

    protected Object convertValue(RDBColumnMetadata column, Term term) {

        return convertValue(column, term.getValue());
    }
}
