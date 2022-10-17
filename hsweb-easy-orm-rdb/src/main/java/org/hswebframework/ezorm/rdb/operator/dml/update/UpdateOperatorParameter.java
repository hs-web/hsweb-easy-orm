package org.hswebframework.ezorm.rdb.operator.dml.update;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.core.param.Term;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

@Getter
@Setter
public class UpdateOperatorParameter {

    Set<UpdateColumn> columns = new LinkedHashSet<>();

    List<Term> where = new ArrayList<>();

}
