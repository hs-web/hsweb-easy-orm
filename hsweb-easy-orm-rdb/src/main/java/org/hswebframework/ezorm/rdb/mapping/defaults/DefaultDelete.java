package org.hswebframework.ezorm.rdb.mapping.defaults;

import org.hswebframework.ezorm.core.NestConditional;
import org.hswebframework.ezorm.core.SimpleNestConditional;
import org.hswebframework.ezorm.core.param.QueryParam;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.events.ContextKeyValue;
import org.hswebframework.ezorm.rdb.mapping.DSLDelete;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.events.EventResultOperator;
import org.hswebframework.ezorm.rdb.mapping.events.MappingEventTypes;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.dml.delete.DeleteOperator;
import org.hswebframework.ezorm.rdb.operator.dml.delete.DeleteResultOperator;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateResultOperator;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.hswebframework.ezorm.rdb.events.ContextKeys.source;
import static org.hswebframework.ezorm.rdb.events.ContextKeys.tableMetadata;
import static org.hswebframework.ezorm.rdb.mapping.events.MappingContextKeys.*;

@SuppressWarnings("all")
public class DefaultDelete<ME extends DSLDelete<?>> implements DSLDelete<ME> {

    protected List<Term> terms = new ArrayList<>();

    protected Accepter<ME, Object> accepter = this::and;

    protected DeleteOperator operator;

    private RDBTableMetadata metadata;
    protected List<ContextKeyValue<?>> contextKeyValues = new ArrayList<>();

    public DefaultDelete(RDBTableMetadata tableMetadata, DeleteOperator operator, ContextKeyValue<?>... mapping) {
        this.operator = operator;
        this.metadata=tableMetadata;
        contextKeyValues.add(source(this));
        contextKeyValues.add(delete(operator));
        contextKeyValues.add(tableMetadata(tableMetadata));
        contextKeyValues.addAll(Arrays.asList(mapping));
    }

    protected DeleteResultOperator doExecute() {
        return EventResultOperator.create(
                () -> {
                    return operator
                            .where(dsl -> terms.forEach(dsl::accept))
                            .execute();
                },
                DeleteResultOperator.class,
                metadata,
                MappingEventTypes.delete_before,
                MappingEventTypes.delete_after,
                contextKeyValues.toArray(new ContextKeyValue[0])
        );
    }

    public QueryParam toQueryParam() {
        QueryParam param = new QueryParam();
        param.setTerms(terms);
        param.setPaging(false);
        return param;
    }

    @Override
    public NestConditional<ME> nest() {
        Term term = new Term();
        term.setType(Term.Type.and);
        terms.add(term);
        return new SimpleNestConditional<>((ME) this, term);
    }

    @Override
    public NestConditional<ME> orNest() {
        Term term = new Term();
        term.setType(Term.Type.or);
        terms.add(term);
        return new SimpleNestConditional<>((ME) this, term);
    }

    @Override
    public ME and() {
        this.accepter = this::and;
        return (ME) this;
    }

    @Override
    public ME or() {
        this.accepter = this::or;
        return (ME) this;
    }

    @Override
    public ME and(String column, String termType, Object value) {
        if (value != null) {
            Term term = new Term();
            term.setColumn(column);
            term.setTermType(termType);
            term.setValue(value);
            term.setType(Term.Type.and);
            terms.add(term);
        }
        return (ME) this;
    }

    @Override
    public ME or(String column, String termType, Object value) {
        if (value != null) {
            Term term = new Term();
            term.setColumn(column);
            term.setTermType(termType);
            term.setValue(value);
            term.setType(Term.Type.or);
            terms.add(term);
        }
        return (ME) this;
    }

    @Override
    public Accepter<ME, Object> getAccepter() {
        return accepter;
    }

    @Override
    public ME accept(Term term) {
        terms.add(term);
        return (ME) this;
    }
}
