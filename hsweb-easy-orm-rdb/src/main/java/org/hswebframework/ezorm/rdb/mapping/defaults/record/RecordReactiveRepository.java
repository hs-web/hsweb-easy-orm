package org.hswebframework.ezorm.rdb.mapping.defaults.record;

import org.hswebframework.ezorm.rdb.mapping.defaults.DefaultReactiveRepository;
import org.hswebframework.ezorm.rdb.mapping.defaults.SimpleColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.events.MappingContextKeys;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.slf4j.Logger;
import reactor.core.publisher.Mono;
import reactor.util.context.Context;

import java.util.function.Supplier;

public class RecordReactiveRepository<K> extends DefaultReactiveRepository<Record, K> {

    public RecordReactiveRepository(DatabaseOperator operator, String table) {
        this(operator, () -> operator
            .getMetadata()
            .getTable(table)
            .orElseThrow(() -> new UnsupportedOperationException("table [" + table + "] doesn't exist")));
    }

    public RecordReactiveRepository(DatabaseOperator operator, Supplier<RDBTableMetadata> table) {
        super(operator, table, Record.class, RecordResultWrapper.of(SimpleColumnMapping.of(DefaultRecord.class, table)));
    }


    @Override
    protected Context applyContext(Context context) {
        if (context.hasKey(Logger.class)) {
            return context;
        }
        return super.applyContext(context);
    }

    @Override
    protected void initMapping(Class<Record> entityType) {

        this.mapping = SimpleColumnMapping.of(entityType, tableSupplier);
        defaultContextKeyValue.add(MappingContextKeys.columnMapping(mapping));

    }
}
