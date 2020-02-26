package org.hswebframework.ezorm.rdb.events;

import org.hswebframework.ezorm.core.FeatureId;
import org.hswebframework.ezorm.core.FeatureType;
import org.hswebframework.ezorm.core.meta.DefaultFeatureType;
import org.hswebframework.ezorm.core.meta.Feature;
import reactor.core.publisher.Mono;

public interface EventListener extends Feature {

    String ID_VALUE = "EventListener";

    @Override
    default String getId() {
        return ID_VALUE;
    }

    @Override
    default String getName() {
        return "事件监听器";
    }

    @Override
    default FeatureType getType() {
        return DefaultFeatureType.eventListener;
    }

    FeatureId<EventListener> ID = FeatureId.of(ID_VALUE);

    void onEvent(EventType type, EventContext context);

}
