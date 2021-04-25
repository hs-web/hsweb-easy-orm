package org.hswebframework.ezorm.rdb.operator.builder.fragments.term;

import lombok.Getter;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.core.param.TermType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;

import java.util.List;

public class EnumFragmentBuilder extends AbstractTermFragmentBuilder {

    public static EnumFragmentBuilder eq = new EnumFragmentBuilder(TermType.eq,false);

    public static EnumFragmentBuilder not = new EnumFragmentBuilder(TermType.not,true);


    @Getter
    private final boolean _not;

    public EnumFragmentBuilder(String termType, boolean not) {
        super(termType, "枚举");
        this._not = not;
    }

    @Override
    public SqlFragments createFragments(String columnFullName, RDBColumnMetadata column, Term term) {
        List<Object> values = convertList(column, term);
        long mask = 0;
        for (Object value : values) {
            if (value instanceof Number) {
                mask |= ((Number) value).longValue();
            }
        }
        return PrepareSqlFragments
                .of(columnFullName)
                .addSql(_not ? "!= ?" : "= ?").addParameter(mask);
    }

}
