package org.hswebframework.ezorm.rdb.operator.dml.query;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.metadata.TableOrViewMetadata;
import org.hswebframework.ezorm.rdb.operator.dml.Join;

/**
 * Explicit query-local table mappings. A caller can retain immutable metadata snapshots for joins
 * without replacing the shared schema or changing existing entity repositories. Only a real binding
 * instance is interpreted; untrusted query context maps cannot manufacture table metadata.
 *
 * @since 4.2.3
 */
public final class QueryTableBindings {
    public static final String CONTEXT_KEY = "easyorm.query.table-bindings";
    private final Map<String, TableOrViewMetadata> tables;
    private final List<Join> joins;

    public QueryTableBindings(Map<String, TableOrViewMetadata> tables) {
        this(tables, Collections.emptyList());
    }

    /** Server-prepared joins are copied when attached to a query; caller mutation is not shared. */
    public QueryTableBindings(Map<String, TableOrViewMetadata> tables, List<Join> joins) {
        this.tables = Map.copyOf(tables);
        this.joins = joins.stream().map(QueryTableBindings::copy).collect(Collectors.toList());
    }

    Optional<TableOrViewMetadata> find(String name) { return Optional.ofNullable(tables.get(name)); }

    List<Join> joins() { return joins.stream().map(QueryTableBindings::copy).collect(Collectors.toList()); }

    private static Join copy(Join source) {
        Join target = new Join();
        target.setTarget(source.getTarget());
        target.setAlias(source.getAlias());
        target.setType(source.getType());
        target.setAliasList(new HashSet<>(source.getAliasList()));
        target.setTerms(source.getTerms().stream().map(Term::clone)
            .collect(Collectors.toList()));
        return target;
    }
}
