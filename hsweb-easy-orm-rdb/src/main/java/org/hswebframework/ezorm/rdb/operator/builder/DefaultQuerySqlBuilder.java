package org.hswebframework.ezorm.rdb.operator.builder;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.meta.DefaultRDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.meta.RDBFutures;
import org.hswebframework.ezorm.rdb.meta.TableOrViewMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.QuerySqlFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.operator.dml.ComplexQueryParameter;

import java.util.Optional;

import static org.hswebframework.ezorm.rdb.meta.RDBFutures.*;

public class DefaultQuerySqlBuilder implements QuerySqlBuilder {

    protected ComplexQueryParameter parameter;

    protected TableOrViewMetadata metadata;

    public DefaultQuerySqlBuilder(ComplexQueryParameter parameter, DefaultRDBSchemaMetadata schema) {
        String from = parameter.getFrom();
        if (from == null || from.isEmpty()) {
            throw new UnsupportedOperationException("from table or view not set");
        }
        this.metadata = schema.findTableOrView(from)
                .orElseThrow(() -> new UnsupportedOperationException("table or view [" + from + "] doesn't exist "));
        this.parameter = parameter;
    }

    protected Optional<SqlFragments> select() {
        return metadata.<QuerySqlFragmentBuilder>getFeature(select)
                .map(builder -> builder.createFragments(parameter))
                .filter(SqlFragments::isNotEmpty);
    }

    protected Optional<SqlFragments> where() {
        return metadata.<QuerySqlFragmentBuilder>getFeature(where)
                .map(builder -> builder.createFragments(parameter))
                .filter(SqlFragments::isNotEmpty);
    }

    protected Optional<SqlFragments> join() {
        return metadata.<QuerySqlFragmentBuilder>getFeature(selectJoin)
                .map(builder -> builder.createFragments(parameter))
                .filter(SqlFragments::isNotEmpty);
    }

    @Override
    public SqlRequest build() {

        PrepareSqlFragments fragments = PrepareSqlFragments.of();

        fragments.addSql("select");

        fragments.addFragments(select().orElseGet(() -> PrepareSqlFragments.of().addSql("*")));

        fragments.addSql("from")
                .addSql(metadata.getFullName())
                .addSql(metadata.getName());

        join().ifPresent(fragments::addFragments);
        //where

        where().ifPresent(where -> fragments.addSql("where").addFragments(where));

        //group by

        //having


        return fragments.toRequest();
    }
}
