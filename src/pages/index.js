import React, {useState, useEffect} from 'react'

const cardBackUrl = 'https://gamesbyjohnny.files.wordpress.com/2009/11/magic-the-gathering-card-back.jpg'

const range = end => end >= 1 ? Array.from(Array(parseInt(end)).keys()) : []

const Card = ({width}) => {
    const [imageUrl, setImageUrl] = useState('')
    const [showTextarea, setShowTextarea] = useState(false)
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

    return (
        <div style={divStyle} onClick={() => setShowTextarea(true)}>
            <textarea onBlur={onBlur} hidden={!showTextarea} ref={textareaRef}></textarea>
        </div>
    )
}

const Category = ({cellWidth}) => {
    const style = {
        width: 'max-content',
        display: 'flex',
        justifyContent: 'center',
        alignItems: 'center',
        flexDirection: 'column',
        borderRight: 'solid',
        padding: 20,
    }
    return (
        <div style={style}>
            <input defaultValue='Title' />
            <table>
                <tbody>
                    <tr>
                        <td><Card width={cellWidth}/></td>
                        <td><Card width={cellWidth}/></td>
                        <td><Card width={cellWidth}/></td>
                    </tr>
                    <tr>
                        <td><Card width={cellWidth}/></td>
                        <td><Card width={cellWidth}/></td>
                        <td><Card width={cellWidth}/></td>
                    </tr>
                    <tr>
                        <td><Card width={cellWidth}/></td>
                        <td><Card width={cellWidth}/></td>
                        <td><Card width={cellWidth}/></td>
                    </tr>
                    <tr>
                        <td><Card width={cellWidth}/></td>
                        <td><Card width={cellWidth}/></td>
                        <td><Card width={cellWidth}/></td>
                    </tr>
                    <tr>
                        <td><Card width={cellWidth}/></td>
                        <td><Card width={cellWidth}/></td>
                        <td><Card width={cellWidth}/></td>
                    </tr>
                </tbody>
            </table>
        </div>
    )
}

export default function Home() {
    const [categories, setCategories] = useState([0])
    return (
        <div>
            <label>number of categories: </label>
            <input value={categories.length}
                onChange={event => {setCategories(range(parseInt(event.target.value)))}}/>
            <div style={{display: 'flex'}}>
                {categories.map(key => <Category cellWidth={120} key={key} />)}
            </div>
        </div>
    )
}
