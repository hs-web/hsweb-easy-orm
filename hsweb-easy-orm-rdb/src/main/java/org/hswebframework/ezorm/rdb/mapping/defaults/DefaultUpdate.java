package org.hswebframework.ezorm.rdb.mapping.defaults;

import org.hswebframework.ezorm.core.*;
import org.hswebframework.ezorm.core.param.Param;
import org.hswebframework.ezorm.core.param.QueryParam;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.core.param.UpdateParam;
import org.hswebframework.ezorm.rdb.events.ContextKeyValue;
import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.ezorm.rdb.mapping.DSLUpdate;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.events.EventResultOperator;
import org.hswebframework.ezorm.rdb.mapping.events.MappingContextKeys;
import org.hswebframework.ezorm.rdb.mapping.events.MappingEventTypes;
import org.hswebframework.ezorm.rdb.metadata.JdbcDataType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateOperator;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateResultOperator;
import org.hswebframework.ezorm.rdb.utils.PropertyUtils;

import java.sql.JDBCType;
import java.util.*;
import java.util.function.Supplier;

import static org.hswebframework.ezorm.rdb.events.ContextKeys.source;
import static org.hswebframework.ezorm.rdb.events.ContextKeys.tableMetadata;
import static org.hswebframework.ezorm.rdb.mapping.events.MappingContextKeys.*;

@SuppressWarnings("all")
public class DefaultUpdate<E, ME extends DSLUpdate<?, ?>> implements DSLUpdate<E, ME> {

    protected List<Term> terms = new ArrayList<>();
    protected Set<String> includes = new HashSet<>();
    protected Set<String> excludes = new HashSet<>();
    protected Accepter<ME, Object> accepter = this::and;

    protected RDBTableMetadata table;

    protected UpdateOperator operator;

    protected EntityColumnMapping mapping;

    protected Set<ContextKeyValue<?>> contextKeyValues = new HashSet<>();

    protected Map<String, Object> tempInstance = new HashMap<>();
    protected E instance;

    public DefaultUpdate(RDBTableMetadata table,
                         UpdateOperator operator,
                         EntityColumnMapping mapping,
                         ContextKeyValue<?>... keyValues) {
        this.table = table;
        this.operator = operator;
        this.mapping = mapping;
        contextKeyValues.add(source(this));
        contextKeyValues.add(update(operator));
        contextKeyValues.add(tableMetadata(table));
        contextKeyValues.add(updateColumnInstance(tempInstance));
        contextKeyValues.addAll(Arrays.asList(keyValues));
    }

    public QueryParam toQueryParam() {
        return toQueryParam(QueryParam::new);
    }

    @Override
    public <T extends QueryParam> T toQueryParam(Supplier<T> template) {
        T param = template.get();
        param.setTerms(terms);
        param.setPaging(false);
        return param;
    }

    protected UpdateResultOperator doExecute() {
        return EventResultOperator.create(
            () -> {
                if (null != instance) {
                    applyColumns(instance);
                }
                for (Map.Entry<String, Object> entry : tempInstance.entrySet()) {
                    if (entry.getValue() != null) {
                        String column = mapping
                            .getColumnByName(entry.getKey())
                            .map(RDBColumnMetadata::getName)
                            .orElse(entry.getKey());
                        if (excludes.contains(column)) {
                            //忽略修改
                            operator.set(column, null);
                        } else {
                            operator.set(column, entry.getValue());
                        }
                    }
                }
                return operator
                    .where(dsl -> terms.forEach(dsl::accept))
                    .execute();
            },
            UpdateResultOperator.class,
            table,
            MappingEventTypes.update_before,
            MappingEventTypes.update_after,
            contextKeyValues.toArray(new ContextKeyValue[0])
        );
    }

    private void applyColumns(E instance) {
        mapping
            .getColumnPropertyMapping()
            .entrySet()
            .stream()
            .filter(e -> includes.isEmpty() || includes.contains(e.getKey()) || includes.contains(e.getValue()))
            .filter(e -> !excludes.contains(e.getKey()) && !excludes.contains(e.getValue()))
            .forEach(e -> PropertyUtils
                .getProperty(instance, e.getValue(), mapping)
                .ifPresent(val -> this.set(e.getKey(), val)));
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
        applyColumns(entity);
        this.instance = entity;
        return (ME) this;
    }

    @Override
    public ME set(String column, Object value) {
        if (value != null) {
            column = mapping
                .getColumnByName(column)
                .map(RDBColumnMetadata::getName)
                .orElse(column);

            operator.set(column, value);
            tempInstance.put(column, value);
        }
        return (ME) this;
    }

    @Override
    public ME setNull(String column) {
        NullValue nullValue = table
            .getColumn(column)
            .map(columnMetadata -> NullValue.of(columnMetadata.getType()))
            .orElseGet(() -> NullValue.of(JdbcDataType.of(JDBCType.VARCHAR, String.class)));
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
            term.setTermType(termType);
            term.setColumn(column);
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
            term.setTermType(termType);
            term.setColumn(column);
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

    @Override
    public ME accept(Param param) {
        if (param instanceof UpdateParam) {
            includes.addAll(param.getIncludes());
            excludes.addAll(param.getExcludes());
            set((E) ((UpdateParam<?>) param).getData());
        }
        return DSLUpdate.super.accept(param);
    }
}
