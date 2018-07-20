package photo

import (
	"log"
	"time"
)

var registerChan chan *Photo
var fetchChan chan *fetchRequest

type fetchRequest struct {
	key string
	ch  (chan<- *Photo)
}

// InitCache initialize photo cache and set up cleanup cron task.
func InitCache() {
	registerChan = make(chan *Photo)
	fetchChan = make(chan *fetchRequest)
	go cacheWorker()
}

func cacheWorker() {
	photos := make(map[string]*Photo)
	tick := time.NewTicker(time.Hour)
	for {
		select {
		case reg := (<-registerChan):
			log.Printf("Image registered: %s", reg.Secret)
			photos[reg.Secret] = reg
		case req := (<-fetchChan):
			if ph, ok := photos[req.key]; ok {
				req.ch <- ph
				log.Printf("Image requested: %s(found)", req.key)
			} else {
				req.ch <- nil
				log.Printf("Image requested: %s(not found)", req.key)
			}
		case <-tick.C:
			// cleanup caches
			cnt := 0
			now := time.Now()
			before := len(photos)
			for key := range photos {
				ph := photos[key]
				if now.Sub(ph.CreatedAt).Hours() > 1.0 {
					delete(photos, key)
				}
				cnt = cnt + 1
			}
			after := len(photos)
			log.Printf("Cache cleanup: %d -> %d (%d images)", before, after, cnt)
		}
	}
}

// Fetch from image cache
func Fetch(key string) *Photo {
	ch := make(chan *Photo)
	defer close(ch)
	req := &fetchRequest{key, ch}
	fetchChan <- req
	photo := <-ch
	return photo
}

// Register a photo
func Register(photo *Photo) {
	registerChan <- photo
}
