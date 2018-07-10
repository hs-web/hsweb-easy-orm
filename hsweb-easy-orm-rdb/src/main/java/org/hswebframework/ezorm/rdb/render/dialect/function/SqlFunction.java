package org.hswebframework.ezorm.rdb.render.dialect.function;

import org.hswebframework.ezorm.rdb.render.dialect.RenderPhase;

/**
 * @author zhouhao
 * @since 3.0.1
 */
public interface SqlFunction {

    String concat = "concat";

    String bitand = "bitand";

    String apply(Param param);

    class Param {
        public static Param of(RenderPhase phase, Object param) {
            Param target = new Param();
            target.phase = phase;
            target.param = param;
            return target;
        }

        private RenderPhase phase;

        private Object param;

        public Object getParam() {
            return param;
        }

        public void setParam(Object param) {
            this.param = param;
        }

        public RenderPhase getPhase() {
            return phase;
        }

        public void setPhase(RenderPhase phase) {
            this.phase = phase;
        }
    }
}
