package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.TableOrViewMetadata;
import org.hswebframework.ezorm.rdb.metadata.key.ForeignKeyMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.term.ForeignKeyTermFragmentBuilder;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import static org.hswebframework.ezorm.rdb.operator.builder.fragments.TermFragmentBuilder.createFeatureId;


public class SimpleTermsFragmentBuilder extends AbstractTermsFragmentBuilder<TableOrViewMetadata> {

    private static final SimpleTermsFragmentBuilder INSTANCE = new SimpleTermsFragmentBuilder();

    private SimpleTermsFragmentBuilder() {

    }

    public static SimpleTermsFragmentBuilder instance() {
        return INSTANCE;
    }

    @Override
    public SqlFragments createTermFragments(TableOrViewMetadata parameter, List<Term> terms) {
        return super.createTermFragments(parameter, terms);
    }

    @Override
    protected SqlFragments createTermFragments(TableOrViewMetadata table, Term term) {
        return createByTable(table, term);
    }

    public static SqlFragments createByTable(TableOrViewMetadata table, Term term) {
        String columnName = term.getColumn();
        if (columnName == null) {
            if (term.getValue() instanceof NativeSql) {
                NativeSql sql = ((NativeSql) term.getValue());
                return SimpleSqlFragments.of(sql.getSql(), sql.getParameters());
            }
            return EmptySqlFragments.INSTANCE;
        }

        if (columnName.contains(".")) {
            String[] arr = columnName.split("[.]");
            if (table.equalsNameOrAlias(arr[0])) {
                columnName = arr[1];
            } else {
                return table
                    .getForeignKey(arr[0])
                    .flatMap(key -> key
                        .getSource()
                        .findFeature(ForeignKeyTermFragmentBuilder.ID)
                        .map(builder -> builder.createFragments(table.getName(), key, createForeignKeyTerm(key, term))))
                    .orElse(EmptySqlFragments.INSTANCE);
            }
        }

        RDBColumnMetadata column = table.getColumn(columnName).orElse(null);
        if (column == null) {
            return EmptySqlFragments.INSTANCE;
        }

        TermFragmentBuilder builder = column
            .findFeature(TermFragmentBuilder.createFeatureId(term.getTermType()))
            .orElse(null);

        if (builder != null) {
            return builder
                .createFragments(column.getQuoteName(), column, term);
        }

        return EmptySqlFragments.INSTANCE;
    }

    static List<Term> createForeignKeyTerm(ForeignKeyMetadata keyMetadata, Term term) {
        Term copy = term.clone();
        //只要是嵌套到外键表的条件则认为是关联表的条件
        term.setTerms(new LinkedList<>());

        return Collections.singletonList(copy);
    }
}
