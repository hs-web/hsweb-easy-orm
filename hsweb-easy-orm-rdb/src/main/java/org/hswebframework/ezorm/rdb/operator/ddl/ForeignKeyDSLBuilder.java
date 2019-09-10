package org.hswebframework.ezorm.rdb.operator.ddl;

import org.hswebframework.ezorm.core.Conditional;
import org.hswebframework.ezorm.core.dsl.Query;
import org.hswebframework.ezorm.core.param.QueryParam;
import org.hswebframework.ezorm.rdb.metadata.ForeignKeyBuilder;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;

import java.util.function.Consumer;

public class ForeignKeyDSLBuilder {

    private RDBTableMetadata table;
    private ForeignKeyBuilder.ForeignKeyBuilderBuilder builder = ForeignKeyBuilder.builder();


    public ForeignKeyDSLBuilder(RDBTableMetadata table) {
        this.table = table;
        this.builder.autoJoin(true);
    }


    public ForeignKeyDSLBuilder name(String name) {
        builder.name(name);
        return this;
    }

    public ForeignKeyDSLBuilder alias(String alias) {
        builder.alias(alias);
        return this;
    }

    public ForeignKeyDSLBuilder target(String source) {
        if (source.contains(".")) {
            String[] arr = source.split("[.]");
            return target(arr[0], arr[1]);
        }
        builder.target(source);
        return this;
    }

    public ForeignKeyDSLBuilder target(String source, String column) {
        builder.target(source).targetColumn(column);
        return this;
    }

    public ForeignKeyDSLBuilder autoJoin(boolean autoJoin) {
        builder.autoJoin(autoJoin);
        return this;
    }

    public ForeignKeyDSLBuilder toMany() {
        builder.autoJoin(false)
                .toMany(true);
        return this;
    }

    public ForeignKeyDSLBuilder condition(Consumer<Conditional<?>> consumer) {
        Query<?, QueryParam> query = Query.of();
        consumer.accept(query);
        builder.terms(query.getParam().getTerms());
        return this;
    }

    public RDBTableMetadata commit() {
        table.addForeignKey(builder.build());
        return table;
    }

}
