package org.hswebframework.ezorm.rdb.mapping.defaults;

import org.hswebframework.ezorm.core.NestConditional;
import org.hswebframework.ezorm.core.SimpleNestConditional;
import org.hswebframework.ezorm.core.TermTypeConditionalSupport;
import org.hswebframework.ezorm.core.param.QueryParam;
import org.hswebframework.ezorm.core.param.SqlTerm;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.events.EventContext;
import org.hswebframework.ezorm.rdb.events.EventType;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.MappingFeatureType;
import org.hswebframework.ezorm.rdb.mapping.DSLQuery;
import org.hswebframework.ezorm.rdb.metadata.TableOrViewMetadata;
import org.hswebframework.ezorm.rdb.metadata.key.ForeignKeyMetadata;
import org.hswebframework.ezorm.rdb.operator.DMLOperator;
import org.hswebframework.ezorm.rdb.operator.dml.SortOrderSupplier;
import org.hswebframework.ezorm.rdb.operator.dml.query.SelectColumn;
import org.hswebframework.ezorm.rdb.operator.dml.query.SortOrder;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;
import java.util.stream.Stream;

@SuppressWarnings("all")
public class DefaultQuery<T, ME extends DSLQuery> implements DSLQuery<ME> {

    protected QueryParam param = new QueryParam();

    protected Term.Type currentTermType = Term.Type.and;

    protected TermTypeConditionalSupport.Accepter<ME, Object> accepter = this::and;

    protected DMLOperator operator;

    protected String tableName;

    protected ResultWrapper<T, ?> wrapper;

    protected TableOrViewMetadata tableMetadata;

    protected EntityColumnMapping columnMapping;

    protected List<SortOrder> orders = new ArrayList<>();

    public DefaultQuery(TableOrViewMetadata tableMetadata, EntityColumnMapping mapping, DMLOperator operator, ResultWrapper<T, ?> wrapper) {
        this.operator = operator;
        this.tableName = tableMetadata.getName();
        this.wrapper = wrapper;
        this.tableMetadata = tableMetadata;
        this.columnMapping = mapping;
        param.setPaging(false);
    }

    @Override
    public ME select(String... columns) {
        param.includes(columns);
        return (ME) this;
    }

    @Override
    public ME selectExcludes(String... columns) {
        param.excludes(columns);
        return (ME) this;
    }

    public ME paging(int pageIndex, int pageSize) {
        param.doPaging(pageIndex, pageSize);
        return (ME) this;
    }

    @Override
    public ME orderBy(SortOrder... orders) {
        this.orders.addAll(Arrays.asList(orders));
        return (ME) this;
    }

    @Override
    public ME orderBy(SortOrderSupplier... orders) {
        return orderBy(Arrays.stream(orders)
                .map(SortOrderSupplier::get)
                .toArray(SortOrder[]::new));
    }

    @Override
    public ME setParam(QueryParam param) {
        this.param = param;
        return (ME) this;
    }

    private boolean isSelectInclude(Map.Entry<String, String> entry) {
        return param.getIncludes().isEmpty() || param.getIncludes().containsAll(Arrays.asList(entry.getKey(), entry.getValue()));
    }

    private boolean isSelectExclude(Map.Entry<String, String> entry) {
        return param.getExcludes().containsAll(Arrays.asList(entry.getKey(), entry.getValue()));
    }

    protected SelectColumn[] getSelectColumn() {

        return Stream.concat(
                tableMetadata.getForeignKeys().stream()
                        .map(key -> key.getAlias() == null ? key.getTarget().getName() : key.getAlias())
                        .map(alias -> alias.concat(".*"))
                        .map(name -> SelectColumn.of(name)), columnMapping.getColumnPropertyMapping()
                        .entrySet()
                        .stream()

                        .filter(this::isSelectInclude)
                        .filter(e -> !isSelectExclude(e))
                        .map(entry -> SelectColumn.of(entry.getKey(), entry.getValue()))
        ).toArray(SelectColumn[]::new);
    }

    protected SortOrder[] getSortOrder() {
        return Stream.concat(
                param.getSorts()
                        .stream()
                        .map(sort -> sort.getOrder().equalsIgnoreCase("asc") ? SortOrder.asc(sort.getName()) : SortOrder.desc(sort.getName()))
                , orders.stream())
                .toArray(SortOrder[]::new);
    }


    @Override
    public NestConditional<ME> nest() {
        return new SimpleNestConditional<>((ME) this, param.nest());
    }

    @Override
    public NestConditional<ME> orNest() {
        return new SimpleNestConditional<>((ME) this, param.orNest());
    }

    @Override
    public ME and() {
        accepter = this::and;
        currentTermType = Term.Type.and;
        return (ME) this;
    }

    @Override
    public ME or() {
        accepter = this::or;
        currentTermType = Term.Type.or;
        return (ME) this;
    }

    @Override
    public ME and(String column, String termType, Object value) {
        if (value != null) {
            param.and(column, termType, value);
        }
        return (ME) this;
    }

    @Override
    public ME or(String column, String termType, Object value) {
        if (value != null) {
            param.or(column, termType, value);
        }
        return (ME) this;
    }

    @Override
    public TermTypeConditionalSupport.Accepter<ME, Object> getAccepter() {
        return accepter;
    }

    @Override
    public ME accept(Term term) {
        param.getTerms().add(term);

        return (ME) this;
    }


}
