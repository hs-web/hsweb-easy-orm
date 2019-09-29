package org.hswebframework.ezorm.rdb.mapping.defaults.record;

import org.hswebframework.ezorm.rdb.mapping.defaults.DefaultSyncRepository;
import org.hswebframework.ezorm.rdb.mapping.defaults.SimpleColumnMapping;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;

import java.util.function.Supplier;

public class RecordSyncRepository<K> extends DefaultSyncRepository<Record, K> {

    public RecordSyncRepository(DatabaseOperator operator, String table) {
        this(operator,()->operator.getMetadata().getTable(table).orElseThrow(()->new UnsupportedOperationException("table [" + table + "] doesn't exist")));
    }

    public RecordSyncRepository(DatabaseOperator operator, Supplier<RDBTableMetadata> table) {
        super(operator, table, Record.class, RecordResultWrapper.of(SimpleColumnMapping.of(table)));
    }

    @Override
    protected void initMapping(Class<Record> entityType) {

        this.mapping = SimpleColumnMapping.of(tableSupplier);

    }
}
