import React, {useState, useEffect} from 'react'

const cardBackUrl = 'https://gamesbyjohnny.files.wordpress.com/2009/11/magic-the-gathering-card-back.jpg'

const Card = ({width}) => {
    const [imageUrl, setImageUrl] = useState('')
    const [showTextarea, setShowTextarea] = useState(true)
    const textareaRef = React.createRef()

    useEffect(() => {
        textareaRef.current.focus()
    }, [textareaRef])

    const onBlur = event => {
        const query = event.target.value
        if(!query) {
            setImageUrl('')
            setShowTextarea(false)
            return
        }
        fetch(`https://api.scryfall.com/cards/search?q=${query}`)
            .then(response => {
                if(response.status === 200) {
                    return response.json()
                } else {
                    throw Error(404)
                }
            })
            .then(json => {
                if (json.data) {
                    const card = json.data[0]
                    setImageUrl((card.image_uris || card.card_faces[0].image_uris).normal)
                    setShowTextarea(false)
                }
            })
            .catch(() => {
                setImageUrl('https://c1.scryfall.com/file/scryfall-cards/normal/front/5/2/52558748-6893-4c72-a9e2-e87d31796b59.jpg?1559959349')
                textareaRef.current.focus()
            })
    }

    const divStyle = {
        width: width,
        height: width*1.39,
        display: 'flex',
        justifyContent: 'center',
        alignItems: 'center',
        backgroundImage: `url(${imageUrl || cardBackUrl})`,
        backgroundSize: 'contain'
    }

    return (<div>
        <div style={divStyle} onClick={() => setShowTextarea(true)}>
            <textarea onBlur={onBlur} hidden={!showTextarea} ref={textareaRef}></textarea>
        </div>
    </div>)
}

export default function Home() {
    return (
        <div>
            <Card width={200}/>
        </div>
    )
}
