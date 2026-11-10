package org.hswebframework.ezorm.rdb.operator.dml.query;

import lombok.Getter;
import lombok.Setter;
import lombok.SneakyThrows;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.operator.dml.*;
import org.hswebframework.ezorm.rdb.metadata.TableOrViewMetadata;

import java.util.*;

@Getter
@Setter
public class QueryOperatorParameter {

    public static final String STRICT_TERM_KEY = "easyorm.strict.term";

    private List<SelectColumn> select = new ArrayList<>();

    /**
     * 别名,用于标识可使用的条件别名对应的列信息.
     *
     * @since 4.1.4
     */
    private List<SelectColumn> alias = new ArrayList<>();

    private Set<String> selectExcludes = new HashSet<>();

    private String from;

    private String fromAlias;

    private List<Term> where = new ArrayList<>(5);

    private List<Join> joins = new ArrayList<>();

    private List<SortOrder> orderBy = new ArrayList<>(2);

    private List<SelectColumn> groupBy = new ArrayList<>(2);

    private List<Term> having = new ArrayList<>();

    private Integer pageIndex;

    private Integer pageSize;

    private Boolean forUpdate;

    private Map<String, Object> context;

    private List<Join> boundJoins = new ArrayList<>();

    public QueryOperatorParameter() {
    }

    /** Attach trusted operation-local joins without changing ordinary explicit join behavior. */
    public void setContext(Map<String, Object> context) {
        joins.removeAll(boundJoins);
        this.context = context;
        Object bindings = context == null ? null : context.get(QueryTableBindings.CONTEXT_KEY);
        boundJoins = bindings instanceof QueryTableBindings
            ? ((QueryTableBindings) bindings).joins() : new ArrayList<>();
        joins.addAll(boundJoins);
    }

    /** Resolves explicit per-operation tables first, retaining the normal schema fallback. */
    public Optional<TableOrViewMetadata> findTable(TableOrViewMetadata owner, String name) {
        Object bindings = context == null ? null : context.get(QueryTableBindings.CONTEXT_KEY);
        if (bindings instanceof QueryTableBindings) {
            Optional<TableOrViewMetadata> selected = ((QueryTableBindings) bindings).find(name);
            if (selected.isPresent()) {
                return selected;
            }
        }
        return owner.getSchema().findTableOrView(name);
    }

    public Optional<Join> findJoin(String targetName) {
        return Optional
            .ofNullable(joins)
            .flatMap(_joins -> _joins
                .stream()
                .filter(join -> join.equalsTargetOrAlias(targetName))
                .findFirst());
    }

    public String getFromAlias() {
        if (fromAlias == null) {
            return from;
        }

        return fromAlias;
    }

    public boolean isStrictTerm() {
        if (context != null && context.containsKey(STRICT_TERM_KEY)) {
            Object value = context.get(STRICT_TERM_KEY);
            if (value instanceof Boolean) {
                return (Boolean) value;
            }
        }
        return false;
    }

    @Override
    @SuppressWarnings("all")
    @SneakyThrows
    public QueryOperatorParameter clone() {
        QueryOperatorParameter parameter = new QueryOperatorParameter();
        parameter.select.addAll(this.select);
        parameter.selectExcludes.addAll(this.selectExcludes);
        parameter.from = this.from;
        parameter.fromAlias = this.fromAlias;
        parameter.alias.addAll(this.alias);
        parameter.where.addAll(this.where);
        parameter.joins.addAll(this.joins);
        parameter.boundJoins.addAll(this.boundJoins);
        parameter.orderBy.addAll(this.orderBy);
        parameter.groupBy.addAll(this.groupBy);
        parameter.having.addAll(this.having);
        parameter.pageIndex = pageIndex;
        parameter.pageSize = pageSize;
        parameter.forUpdate = forUpdate;
        if (context != null) {
            parameter.context = new HashMap<>(context);
        }

        return parameter;
    }
}
