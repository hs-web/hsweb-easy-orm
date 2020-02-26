package org.hswebframework.ezorm.rdb.operator.dml.delete;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.core.param.Term;

import java.util.ArrayList;
import java.util.List;

@Getter
@Setter
public class DeleteOperatorParameter {

    private List<Term> where = new ArrayList<>();


}
