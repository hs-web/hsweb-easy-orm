package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.meta.RDBFutures;
import org.hswebframework.ezorm.rdb.meta.TableOrViewMetadata;
import org.hswebframework.ezorm.rdb.operator.dml.ComplexQueryParameter;
import org.hswebframework.ezorm.rdb.operator.dml.Join;
import org.hswebframework.ezorm.rdb.operator.dml.JoinType;

import java.util.List;

import static java.util.Optional.*;

@AllArgsConstructor(staticName = "of")
public class JoinFragmentBuilder implements QuerySqlFragmentBuilder {

    private TableOrViewMetadata metadata;

    @Override
    public SqlFragments createFragments(ComplexQueryParameter parameter) {

        PrepareSqlFragments fragments = PrepareSqlFragments.of();

        List<Join> joins = parameter.getJoins();

        for (Join join : joins) {
            metadata.getSchema()
                    .findTableOrView(join.getTarget())
                    .ifPresent(target -> {
                        ofNullable(join.getType())
                                .map(JoinType::name)
                                .ifPresent(fragments::addSql);
                        //join schema.table on
                        fragments.addSql("join")
                                .addSql(target.getFullName())
                                .addSql(join.getAlias())
                                .addSql("on");

                        fragments.addFragments(
                                target.<QuerySqlFragmentBuilder>getFeature(RDBFutures.where)
                                        .map(builder -> {

                                            ComplexQueryParameter joinOnParameter = new ComplexQueryParameter();
                                            joinOnParameter.setFrom(target.getName());
                                            joinOnParameter.setFromAlias(join.getAlias());
                                            joinOnParameter.getWhere().addAll(join.getTerms());

                                            return builder.createFragments(joinOnParameter);
                                        })
                                        .filter(SqlFragments::isNotEmpty)
                                        .orElseThrow(() -> new IllegalArgumentException("join terms is empty"))
                        );
                    });

        }

        return fragments;
    }

    @Override
    public String getId() {
        return RDBFutures.selectJoin;
    }

    @Override
    public String getName() {
        return "表连接";
    }
}
