package org.hswebframework.ezorm.rdb.operator.builder;

import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.meta.DefaultRDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.meta.RDBFeatureType;
import org.hswebframework.ezorm.rdb.meta.TableOrViewMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.*;
import org.hswebframework.ezorm.rdb.operator.dml.ComplexQueryParameter;

import java.util.Optional;

import static org.hswebframework.ezorm.rdb.meta.RDBFutures.*;

public class DefaultQuerySqlBuilder implements QuerySqlBuilder {

    protected DefaultRDBSchemaMetadata schema;

    public DefaultQuerySqlBuilder(DefaultRDBSchemaMetadata schema) {
        this.schema = schema;
    }

    protected Optional<SqlFragments> select(ComplexQueryParameter parameter, TableOrViewMetadata metadata) {
        return metadata.<QuerySqlFragmentBuilder>getFeature(select)
                .map(builder -> builder.createFragments(parameter))
                .filter(SqlFragments::isNotEmpty);
    }

    protected Optional<SqlFragments> where(ComplexQueryParameter parameter, TableOrViewMetadata metadata) {
        return metadata.<QuerySqlFragmentBuilder>getFeature(where)
                .map(builder -> builder.createFragments(parameter))
                .filter(SqlFragments::isNotEmpty);
    }

    protected Optional<SqlFragments> join(ComplexQueryParameter parameter, TableOrViewMetadata metadata) {
        return metadata.<QuerySqlFragmentBuilder>getFeature(selectJoin)
                .map(builder -> builder.createFragments(parameter))
                .filter(SqlFragments::isNotEmpty);
    }

    protected SqlRequest build(TableOrViewMetadata metadata, ComplexQueryParameter parameter) {
        BlockSqlFragments fragments = BlockSqlFragments.of();

        fragments.addBlock(FragmentBlock.before, "select");

        fragments.addBlock(FragmentBlock.selectColumn, select(parameter, metadata)
                .orElseGet(() -> PrepareSqlFragments.of().addSql("*")));

        fragments.addBlock(FragmentBlock.selectFrom, PrepareSqlFragments.of()
                .addSql("from")
                .addSql(metadata.getFullName())
                .addSql(parameter.getFromAlias()));


        join(parameter, metadata)
                .ifPresent(join -> fragments.addBlock(FragmentBlock.join, join));
        //where

        where(parameter, metadata)
                .ifPresent(where ->
                        fragments.addBlock(FragmentBlock.where,"where")
                                .addBlock(FragmentBlock.where, where));

        //group by

        //having

        if (Boolean.TRUE.equals(parameter.getForUpdate())) {
            fragments.addBlock(FragmentBlock.after, PrepareSqlFragments.of().addSql("for update"));
        }
        //分页
        else if (parameter.getPageIndex() != null && parameter.getPageSize() != null) {
            return metadata.<Paginator>findFeature(RDBFeatureType.paginator.getId())
                    .map(paginator -> paginator.doPaging(fragments, parameter.getPageIndex(), parameter.getPageSize()))
                    .map(SqlFragments::toRequest)
                    .orElseGet(fragments::toRequest);

        }
        return fragments.toRequest();
    }

    @Override
    public SqlRequest build(ComplexQueryParameter parameter) {
        String from = parameter.getFrom();
        if (from == null || from.isEmpty()) {
            throw new UnsupportedOperationException("from table or view not set");
        }
        TableOrViewMetadata metadata = schema.findTableOrView(from)
                .orElseThrow(() -> new UnsupportedOperationException("table or view [" + from + "] doesn't exist "));

        return build(metadata, parameter);
    }
}
