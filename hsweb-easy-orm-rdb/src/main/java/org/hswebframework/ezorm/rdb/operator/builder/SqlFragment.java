package org.hswebframework.ezorm.rdb.operator.builder;

import org.hswebframework.ezorm.rdb.operator.dml.ComplexQueryParameter;

public interface SqlFragment {

     PrepareSqlFragment getFragment(ComplexQueryParameter parameter);

}
