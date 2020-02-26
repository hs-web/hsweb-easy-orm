package org.hswebframework.ezorm.rdb.supports.oracle;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.hswebframework.ezorm.core.CastUtil;
import org.hswebframework.ezorm.core.meta.ObjectMetadata;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ColumnWrapperContext;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.metadata.RDBIndexMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.parser.IndexMetadataParser;

import java.util.*;
import java.util.stream.Collectors;

import static org.hswebframework.ezorm.rdb.executor.SqlRequests.prepare;
import static org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers.*;

@Slf4j
@AllArgsConstructor(staticName = "of")
public class OracleIndexMetadataParser implements IndexMetadataParser {

    private static final String sql = "select idx.index_name," +
            "idx.table_name," +
            "idx.uniqueness," +
            "col.column_name," +
            "col.column_position," +
            "col.descend from all_ind_columns col " +
            "join all_indexes idx on col.index_name = idx.index_name " +
            "where idx.table_owner=? and upper(idx.table_name)= ?";

    private static final String virtualColumnSql = "select column_name,data_default from all_tab_cols" +
            " where owner=? and upper(table_name) = ? and virtual_column='YES'";

    private static final String primaryKeyIndexSql = "select index_name from all_constraints where " +
            "owner=? and upper(table_name) = ? and constraint_type = 'P'";

    private RDBSchemaMetadata schema;

    @Override
    public List<RDBIndexMetadata> parseTableIndex(String tableName) {
        String schemaName = schema.getName().toUpperCase();
        String tableUpperName = tableName.toUpperCase();

        return schema.<SyncSqlExecutor>findFeature(SyncSqlExecutor.ID)
                .map(sqlExecutor -> sqlExecutor.select(prepare(sql, schemaName, tableUpperName),
                        new OracleIndexWrapper(
                                sqlExecutor
                                        .select(prepare(virtualColumnSql, schemaName, tableUpperName), lowerCase(mapStream()))
                                        .map(CastUtil::<Map<String, String>>cast)
                                        .collect(Collectors.toMap(map -> map.get("column_name"), map -> map.get("data_default")))
                                ,
                                sqlExecutor
                                        .select(prepare(primaryKeyIndexSql, schemaName, tableUpperName),
                                                stream(column("index_name", String::valueOf)))
                                        .collect(Collectors.toSet())
                        )))
                .orElseGet(() -> {
                    log.warn("unsupported SyncSqlExecutor");
                    return Collections.emptyList();
                });
    }

    @Override
    public <T extends ObjectMetadata> Optional<T> parseByName(String name) {
        return Optional.empty();
    }

    @Override
    public <T extends ObjectMetadata> List<T> parseAll() {
        return Collections.emptyList();
    }

    class OracleIndexWrapper implements ResultWrapper<Map<String, String>, List<RDBIndexMetadata>> {
        private Map<String, RDBIndexMetadata> mappingByName = new HashMap<>();
        private Map<String, String> virtualColumn;
        private Set<String> indexName;

        public OracleIndexWrapper(Map<String, String> virtualColumn, Set<String> indexName) {
            this.virtualColumn = virtualColumn;
            this.indexName = indexName;
        }

        @Override
        public Map<String, String> newRowInstance() {
            return new HashMap<>();
        }

        @Override
        public void wrapColumn(ColumnWrapperContext<Map<String, String>> context) {
            if (context.getResult() == null) {
                return;
            }
            context.getRowInstance().put(context.getColumnLabel().toLowerCase(), String.valueOf(context.getResult()));
        }

        @Override
        public boolean completedWrapRow(Map<String, String> result) {
            RDBIndexMetadata metadata = mappingByName.computeIfAbsent(result.get("index_name"), RDBIndexMetadata::new);
            metadata.setTableName(result.get("table_name"));
            metadata.setUnique("UNIQUE".equals(result.get("uniqueness")));
            metadata.setPrimaryKey(indexName.contains(metadata.getName()));
            RDBIndexMetadata.IndexColumn column = new RDBIndexMetadata.IndexColumn();
            column.setSort("ASC".equalsIgnoreCase(result.get("descend")) ?
                    RDBIndexMetadata.IndexSort.asc :
                    RDBIndexMetadata.IndexSort.desc);
            column.setSortIndex(Integer.parseInt(result.get("column_position")));
            String columnName = result.get("column_name");

            column.setColumn(schema.getDialect().clearQuote(virtualColumn.getOrDefault(columnName, columnName)).toLowerCase());

            metadata.getColumns().add(column);
            return true;
        }

        @Override
        public List<RDBIndexMetadata> getResult() {
            return new ArrayList<>(mappingByName.values());
        }
    }
}
