package org.hswebframework.ezorm.rdb.supports.kingbase;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ColumnWrapperContext;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.metadata.RDBIndexMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.parser.IndexMetadataParser;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.util.function.Tuple2;
import reactor.util.function.Tuples;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.hswebframework.ezorm.rdb.executor.SqlRequests.prepare;
import static org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers.column;
import static org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers.stream;

/**
 * Reads the PostgreSQL-compatible KingbaseES index catalog for both database modes.
 */
@Slf4j
public class KingbaseIndexMetadataParser implements IndexMetadataParser {

    private static final String INDEX_SQL = String.join(" ",
        "select i.relname as index_name,",
        "t.relname as table_name,",
        "ix.indisunique as uniqueness,",
        "a.attname as column_name,",
        "k.pos as column_position,",
        "case when (ix.indoption[k.pos-1] & 1) = 1 then 'DESC' else 'ASC' end as descend",
        "from pg_class t",
        "join pg_index ix on t.oid = ix.indrelid",
        "join pg_class i on i.oid = ix.indexrelid",
        "join pg_namespace n on n.oid = t.relnamespace",
        "cross join lateral unnest(ix.indkey) with ordinality as k(attnum, pos)",
        "join pg_attribute a on a.attrelid = t.oid and a.attnum = k.attnum",
        "where n.nspname = ?",
        "and t.relname like ?");

    private static final String PRIMARY_KEY_INDEX_SQL = String.join(" ",
        "select c.relname as index_name",
        "from pg_index ix",
        "join pg_class c on c.oid = ix.indexrelid",
        "where ix.indisprimary",
        "and ix.indrelid in (select oid from pg_class where relnamespace = (select oid from pg_namespace where nspname = ?) and relname like ?)");

    private static final String INDEX_BY_NAME_SQL = String.join(" ",
        "select i.relname as index_name,",
        "t.relname as table_name,",
        "ix.indisunique as uniqueness,",
        "a.attname as column_name,",
        "k.pos as column_position,",
        "case when (ix.indoption[k.pos-1] & 1) = 1 then 'DESC' else 'ASC' end as descend",
        "from pg_class t",
        "join pg_index ix on t.oid = ix.indrelid",
        "join pg_class i on i.oid = ix.indexrelid",
        "join pg_namespace n on n.oid = t.relnamespace",
        "cross join lateral unnest(ix.indkey) with ordinality as k(attnum, pos)",
        "join pg_attribute a on a.attrelid = t.oid and a.attnum = k.attnum",
        "where n.nspname = ?",
        "and i.relname = ?");

    private static final String PRIMARY_KEY_INDEX_BY_NAME_SQL = String.join(" ",
        "select c.relname as index_name",
        "from pg_index ix",
        "join pg_class c on c.oid = ix.indexrelid",
        "join pg_namespace n on n.oid = c.relnamespace",
        "where ix.indisprimary",
        "and n.nspname = ?",
        "and c.relname = ?");

    @Getter
    private final RDBSchemaMetadata schema;

    public KingbaseIndexMetadataParser(RDBSchemaMetadata schema) {
        this.schema = schema;
    }

    @Override
    public List<RDBIndexMetadata> parseTableIndex(String tableName) {
        return select(INDEX_SQL, PRIMARY_KEY_INDEX_SQL, tableName);
    }

    @Override
    public Optional<RDBIndexMetadata> parseByName(String name) {
        return select(INDEX_BY_NAME_SQL, PRIMARY_KEY_INDEX_BY_NAME_SQL, name).stream().findFirst();
    }

    @Override
    public List<RDBIndexMetadata> parseAll() {
        return parseTableIndex("%%");
    }

    @Override
    public Flux<RDBIndexMetadata> parseAllReactive() {
        return parseTableIndexReactive("%%");
    }

    @Override
    public Mono<RDBIndexMetadata> parseByNameReactive(String name) {
        return selectReactive(INDEX_BY_NAME_SQL, PRIMARY_KEY_INDEX_BY_NAME_SQL, name).singleOrEmpty();
    }

    @Override
    public Flux<RDBIndexMetadata> parseTableIndexReactive(String tableName) {
        return selectReactive(INDEX_SQL, PRIMARY_KEY_INDEX_SQL, tableName);
    }

    private List<RDBIndexMetadata> select(String indexSql, String primaryKeySql, String value) {
        String schemaName = schema.getName();
        return schema.<SyncSqlExecutor>findFeature(SyncSqlExecutor.ID)
            .map(executor -> executor.select(
                prepare(indexSql, schemaName, value),
                new IndexWrapper(executor.select(
                    prepare(primaryKeySql, schemaName, value),
                    stream(column("index_name", String::valueOf))
                ).collect(Collectors.toSet()))
            ))
            .orElseGet(() -> {
                log.warn("unsupported SyncSqlExecutor");
                return Collections.emptyList();
            });
    }

    private Flux<RDBIndexMetadata> selectReactive(String indexSql, String primaryKeySql, String value) {
        ReactiveSqlExecutor executor = schema.findFeatureNow(ReactiveSqlExecutor.ID);
        String schemaName = schema.getName();
        return executor.select(prepare(primaryKeySql, schemaName, value), column("index_name", String::valueOf))
            .collect(Collectors.toSet())
            .flatMap(primaryKeys -> {
                IndexWrapper wrapper = new IndexWrapper(primaryKeys);
                return executor.select(prepare(indexSql, schemaName, value), wrapper)
                    .then(Mono.fromSupplier(wrapper::getResult));
            })
            .flatMapIterable(Function.identity());
    }

    private final class IndexWrapper implements ResultWrapper<Map<String, Object>, List<RDBIndexMetadata>> {

        private final Map<Tuple2<String, String>, RDBIndexMetadata> indexes = new HashMap<>();
        private final Set<String> primaryKeyNames;

        private IndexWrapper(Set<String> primaryKeyNames) {
            this.primaryKeyNames = primaryKeyNames;
        }

        @Override
        public Map<String, Object> newRowInstance() {
            return new HashMap<>();
        }

        @Override
        public void wrapColumn(ColumnWrapperContext<Map<String, Object>> context) {
            context.getRowInstance().put(
                context.getColumnLabel().toLowerCase(Locale.ROOT),
                context.getResult()
            );
        }

        @Override
        public boolean completedWrapRow(Map<String, Object> result) {
            String indexName = String.valueOf(result.get("index_name"));
            String tableName = String.valueOf(result.get("table_name"));
            RDBIndexMetadata metadata = indexes.computeIfAbsent(
                Tuples.of(tableName, indexName),
                key -> new RDBIndexMetadata(key.getT2())
            );
            metadata.setTableName(tableName);
            metadata.setUnique(booleanValue(result.get("uniqueness")));
            metadata.setPrimaryKey(primaryKeyNames.contains(metadata.getName()));

            RDBIndexMetadata.IndexColumn column = new RDBIndexMetadata.IndexColumn();
            column.setSort("DESC".equalsIgnoreCase(String.valueOf(result.get("descend")))
                ? RDBIndexMetadata.IndexSort.desc
                : RDBIndexMetadata.IndexSort.asc);
            column.setSortIndex(Integer.parseInt(String.valueOf(result.get("column_position"))));
            column.setColumn(schema.getDialect().clearQuote(String.valueOf(result.get("column_name"))).toLowerCase(Locale.ROOT));
            metadata.getColumns().add(column);
            return true;
        }

        @Override
        public List<RDBIndexMetadata> getResult() {
            return new ArrayList<>(indexes.values());
        }
    }

    private static boolean booleanValue(Object value) {
        if (value instanceof Boolean) {
            return (Boolean) value;
        }
        String text = String.valueOf(value);
        return "1".equals(text) || "t".equalsIgnoreCase(text) || "true".equalsIgnoreCase(text);
    }
}
