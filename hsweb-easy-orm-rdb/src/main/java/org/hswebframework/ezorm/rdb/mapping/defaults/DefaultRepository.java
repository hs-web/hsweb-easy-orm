package org.hswebframework.ezorm.rdb.mapping.defaults;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.core.GlobalConfig;
import org.hswebframework.ezorm.core.ObjectPropertyOperator;
import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.mapping.EntityColumnMapping;
import org.hswebframework.ezorm.rdb.mapping.MappingFeatureType;
import org.hswebframework.ezorm.rdb.mapping.events.MappingEventTypes;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertOperator;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertResultOperator;

import java.util.Collection;
import java.util.Map;
import java.util.stream.Stream;

import static org.hswebframework.ezorm.rdb.events.ContextKeys.tableMetadata;
import static org.hswebframework.ezorm.rdb.mapping.events.MappingContextKeys.*;

public class DefaultRepository<E> {

    protected RDBTableMetadata table;

    protected DatabaseOperator operator;

    protected ResultWrapper<E, ?> wrapper;

    protected String idColumn;

    protected EntityColumnMapping mapping;

    protected String[] properties;

    @Getter
    @Setter
    private ObjectPropertyOperator propertyOperator = GlobalConfig.getPropertyOperator();

    public DefaultRepository(DatabaseOperator operator, RDBTableMetadata table, ResultWrapper<E, ?> wrapper) {
        this.operator = operator;
        this.table = table;
        this.wrapper = wrapper;
    }

    protected void initMapping(Class<E> entityType) {
        this.idColumn = table.getColumns().stream()
                .filter(RDBColumnMetadata::isPrimaryKey)
                .findFirst()
                .map(RDBColumnMetadata::getName)
                .orElse(null);

        this.mapping = table.<EntityColumnMapping>findFeature(MappingFeatureType.columnPropertyMapping.createFeatureId(entityType))
                .orElseThrow(() -> new UnsupportedOperationException("unsupported columnPropertyMapping feature"));

        this.properties = mapping.getColumnPropertyMapping()
                .entrySet()
                .stream()
                .filter(kv -> table.getColumn(kv.getKey()).isPresent())
                .map(Map.Entry::getValue)
                .toArray(String[]::new);

    }

    protected InsertResultOperator doInsert(E data) {

        InsertOperator insert = operator.dml().insert(table.getFullName());

        table.fireEvent(MappingEventTypes.insert_before,
                ctx -> ctx.set(instance(data), type("single"), tableMetadata(table), insert(insert)));

        for (Map.Entry<String, String> entry : mapping.getColumnPropertyMapping().entrySet()) {
            String column = entry.getKey();
            String property = entry.getValue();
            propertyOperator.getProperty(data, property)
                    .ifPresent(val -> insert.value(column, val));

        }

        return insert.execute();

    }

    protected InsertResultOperator doInsert(Collection<E> batch) {
        InsertOperator insert = operator.dml().insert(table.getFullName());

        table.fireEvent(MappingEventTypes.insert_before,
                ctx -> ctx.set(instance(batch), type("batch"), tableMetadata(table), insert(insert)));

        insert.columns(properties);

        for (E e : batch) {
            insert.values(Stream.of(properties)
                    .map(property -> propertyOperator
                            .getProperty(e, property)
                            .orElseGet(() -> mapping.getColumnByProperty(property)
                                    .map(column -> NullValue.of(column.getJavaType(), column.getType()))
                                    .orElse(null)))
                    .toArray());
        }
        return insert.execute();
    }

}
