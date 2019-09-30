package org.hswebframework.ezorm.rdb.operator.ddl;

import org.hswebframework.ezorm.core.Conditional;
import org.hswebframework.ezorm.core.dsl.Query;
import org.hswebframework.ezorm.core.param.QueryParam;
import org.hswebframework.ezorm.rdb.metadata.key.ForeignKeyBuilder;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;

import java.util.function.Consumer;

public class ForeignKeyDSLBuilder {

    private RDBTableMetadata table;
    private ForeignKeyBuilder builder = ForeignKeyBuilder.builder().build();


    public ForeignKeyDSLBuilder(RDBTableMetadata table) {
        this.table = table;
        this.builder.setAutoJoin(true);
    }


    public ForeignKeyDSLBuilder name(String name) {
        builder.setName(name);
        return this;
    }

    public ForeignKeyDSLBuilder alias(String alias) {
        builder.setAlias(alias);
        return this;
    }

    public ForeignKeyDSLBuilder target(String source) {

        builder.setTarget(source);
        return this;
    }

    public ForeignKeyDSLBuilder column(String sourceColumn, String targetColumn) {
        builder.addColumn(sourceColumn,targetColumn);
        return this;
    }

    public ForeignKeyDSLBuilder autoJoin(boolean autoJoin) {
        builder.setAutoJoin(autoJoin);
        return this;
    }

    public ForeignKeyDSLBuilder toMany() {
        builder.setAutoJoin(false);
        builder.setToMany(true);
        return this;
    }

    public ForeignKeyDSLBuilder condition(Consumer<Conditional<?>> consumer) {
        Query<?, QueryParam> query = Query.of();
        consumer.accept(query);
        builder.setTerms(query.getParam().getTerms());
        return this;
    }

    public RDBTableMetadata commit() {
        table.addForeignKey(builder);
        return table;
    }

}
