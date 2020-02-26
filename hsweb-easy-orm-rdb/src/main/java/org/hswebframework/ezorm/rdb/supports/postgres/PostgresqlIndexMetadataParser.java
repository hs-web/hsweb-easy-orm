package org.hswebframework.ezorm.rdb.supports.postgres;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ColumnWrapperContext;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.metadata.RDBIndexMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.parser.IndexMetadataParser;

import java.util.*;

@AllArgsConstructor
@Slf4j
@SuppressWarnings("all")
public class PostgresqlIndexMetadataParser implements IndexMetadataParser {

    @Getter
    private RDBSchemaMetadata schema;

    private static final String all = "select " +
            "A.SCHEMANAME::varchar," +
            " A.TABLENAME::varchar," +
            " A.INDEXNAME::varchar," +
            " IA.ATTNAME::varchar," +
            " IA.ATTNUM::int2," +
            " A.TABLESPACE::varchar," +
            " B.AMNAME::varchar," +
            " C.INDNATTS::varchar," +
            " C.INDISUNIQUE::boolean," +
            " C.INDISPRIMARY::boolean,"+
            " C.INDISCLUSTERED::boolean," +
            " D.DESCRIPTION::varchar"+
            " from PG_AM B" +
            " left join PG_CLASS F on B.OID = F.RELAM" +
            " left join PG_STAT_ALL_INDEXES E on F.OID = E.INDEXRELID" +
            " left join PG_INDEX C on E.INDEXRELID = C.INDEXRELID" +
            " left outer join PG_DESCRIPTION D on C.INDEXRELID = D.OBJOID" +
            " left join pg_attribute IA on IA.attrelid = F.oid" +
            " ," +
            " PG_INDEXES A" +
            " where A.SCHEMANAME = E.SCHEMANAME" +
            " and A.TABLENAME = E.RELNAME" +
            " and A.INDEXNAME = E.INDEXRELNAME" +
            " and E.SCHEMANAME = ?";


    private static final String sql = all +" and E.RELNAME = ?";

    private static final String byName = all + " and A.INDEXNAME = ?";


    @Override
    public List<RDBIndexMetadata> parseTableIndex(String tableName) {
        return doSelect(SqlRequests.of(sql, schema.getName(), tableName));
    }

    @Override
    public Optional<RDBIndexMetadata> parseByName(String name) {
        return doSelect(SqlRequests.of(byName, schema.getName(), name))
                .stream()
                .findFirst();
    }

    @Override
    public List<RDBIndexMetadata> parseAll() {
        return doSelect(SqlRequests.of(all, schema.getName()));
    }

    protected List<RDBIndexMetadata> doSelect(SqlRequest sqlRequest) {
        return schema.<SyncSqlExecutor>findFeature(SyncSqlExecutor.ID)
                .map(sqlExecutor -> sqlExecutor.select(sqlRequest, new PostgresqlIndexMetadataWrapper()))
                .orElseGet(() -> {
                    log.warn("unsupported SyncSqlExecutor");
                    return Collections.emptyList();
                });
    }

    class PostgresqlIndexMetadataWrapper implements ResultWrapper<Map<String, Object>, List<RDBIndexMetadata>> {
        Map<String, RDBIndexMetadata> group = new LinkedHashMap<>();

        @Override
        public Map<String, Object> newRowInstance() {
            return new HashMap<>();
        }

        @Override
        public void wrapColumn(ColumnWrapperContext<Map<String, Object>> context) {
            context.getRowInstance().put(context.getColumnLabel().toLowerCase(), context.getResult());
        }

        @Override
        public boolean completedWrapRow(Map<String, Object> result) {
            String name = (String) result.get("indexname");
            RDBIndexMetadata index = group.computeIfAbsent(name, __ -> new RDBIndexMetadata());
            index.setName(name.toLowerCase());
            index.setTableName(((String) result.get("tablename")).toLowerCase());
            index.setPrimaryKey(Boolean.TRUE.equals(result.get("indisprimary")));
            index.setUnique(Boolean.FALSE.equals(result.get("indisunique")));
            RDBIndexMetadata.IndexColumn indexColumn = new RDBIndexMetadata.IndexColumn();
            indexColumn.setColumn(((String) result.get("attname")).toLowerCase());
            // TODO: 2019-10-22 咋获取排序...
            indexColumn.setSort(  RDBIndexMetadata.IndexSort.asc );
            indexColumn.setSortIndex(((Number) result.get("attnum")).intValue());
            index.getColumns().add(indexColumn);
            return true;
        }


        @Override
        public List<RDBIndexMetadata> getResult() {
            return new ArrayList<>(group.values());
        }
    }
}
