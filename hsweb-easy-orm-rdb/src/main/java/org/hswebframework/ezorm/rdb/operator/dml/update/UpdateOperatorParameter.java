package org.hswebframework.ezorm.rdb.operator.dml.update;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.core.param.Term;

import java.util.ArrayList;
import java.util.List;

@Getter
@Setter
public class UpdateOperatorParameter {

    List<UpdateColumn> columns = new ArrayList<>();

    List<Term> where = new ArrayList<>();

}
