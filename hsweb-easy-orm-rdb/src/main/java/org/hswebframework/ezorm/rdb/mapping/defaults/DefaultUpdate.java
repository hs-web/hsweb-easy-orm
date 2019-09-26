package org.hswebframework.ezorm.rdb.mapping.defaults;

import org.hswebframework.ezorm.core.NestConditional;
import org.hswebframework.ezorm.core.ObjectPropertyOperator;
import org.hswebframework.ezorm.core.SimpleNestConditional;
import org.hswebframework.ezorm.core.param.QueryParam;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.core.GlobalConfig;
import org.hswebframework.ezorm.rdb.events.ContextKeyValue;
import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.ezorm.rdb.mapping.DSLUpdate;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.MappingFeatureType;
import org.hswebframework.ezorm.rdb.mapping.events.MappingContextKeys;
import org.hswebframework.ezorm.rdb.mapping.events.MappingEventTypes;
import org.hswebframework.ezorm.rdb.metadata.JdbcDataType;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateOperator;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateResultOperator;

import java.sql.JDBCType;
import java.util.*;

import static org.hswebframework.ezorm.rdb.events.ContextKeys.source;
import static org.hswebframework.ezorm.rdb.events.ContextKeys.tableMetadata;
import static org.hswebframework.ezorm.rdb.mapping.events.MappingContextKeys.*;

@SuppressWarnings("all")
public class DefaultUpdate<E, ME extends DSLUpdate> implements DSLUpdate<E, ME> {

    protected List<Term> terms = new ArrayList<>();
    protected Set<String> includes = new HashSet<>();
    protected Set<String> excludes = new HashSet<>();
    protected Accepter<ME, Object> accepter = this::and;

    protected RDBTableMetadata table;

    protected UpdateOperator operator;

    protected ObjectPropertyOperator propertyOperator = GlobalConfig.getPropertyOperator();

    protected EntityColumnMapping mapping;

    protected List<ContextKeyValue<?>> contextKeyValues = new ArrayList<>();

    protected Map<String, Object> tempInstance = new HashMap<>();

    public DefaultUpdate(RDBTableMetadata table, UpdateOperator operator, EntityColumnMapping mapping) {
        this.table = table;
        this.operator = operator;
        this.mapping = mapping;
    }

    public QueryParam toQueryParam() {
        QueryParam param = new QueryParam();
        param.setTerms(terms);
        return param;
    }

    protected UpdateResultOperator doExecute() {
        return operator
                .where(dsl -> terms.forEach(dsl::accept))
                .accept(operator ->
                        table.fireEvent(MappingEventTypes.update_before, eventContext ->
                                eventContext.set(
                                        source(DefaultUpdate.this),
                                        update(operator),
                                        tableMetadata(table)
                                ).set(contextKeyValues.toArray(new ContextKeyValue[0]))))
                .execute();
    }

    @Override
    public ME includes(String... properties) {
        includes.addAll(Arrays.asList(properties));
        return (ME) this;
    }

    @Override
    public ME excludes(String... properties) {
        excludes.addAll(Arrays.asList(properties));
        return (ME) this;
    }

    @Override
    public ME set(E entity) {
        contextKeyValues.add(MappingContextKeys.instance(entity));

        mapping.getColumnPropertyMapping()
                .entrySet()
                .stream()
                .filter(e -> includes.isEmpty() || includes.contains(e.getKey()) || includes.contains(e.getValue()))
                .filter(e -> !excludes.contains(e.getKey()) && !excludes.contains(e.getValue()))
                .forEach(e -> propertyOperator.getProperty(entity, e.getValue()).ifPresent(val -> this.set(e.getKey(), val)));
        return (ME) this;
    }

    @Override
    public ME set(String column, Object value) {
        if (value != null) {
            operator.set(column, value);
        }
        return (ME) this;
    }

    @Override
    public ME setNull(String column) {
        NullValue nullValue = table.getColumn(column)
                .map(columnMetadata -> NullValue.of(columnMetadata.getJavaType(), columnMetadata.getType()))
                .orElseGet(() -> NullValue.of(String.class, JdbcDataType.of(JDBCType.VARCHAR, String.class)));
        set(column, nullValue);
        return (ME) this;
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
